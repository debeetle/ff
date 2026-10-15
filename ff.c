#define _DEFAULT_SOURCE
#define _DARWIN_C_SOURCE
#define _XOPEN_SOURCE 700

#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <termios.h>
#include <unistd.h>

enum {
	COST_NEIGHBOR_SUBSTITUTION = 3,
	COST_TRANSPOSITION = 5,
	COST_INSERTION = 8,
	COST_DELETION = 8,
	COST_SUBSTITUTION = 10,
	MAX_EDITS = 2,
	EDIT_SLOTS = MAX_EDITS + 1,
	MAX_QUERY = 256,
};

typedef struct {
	char *raw;
	char *folded;
	size_t length;
} candidate_t;

typedef struct {
	candidate_t *items;
	size_t length;
	size_t capacity;
} candidate_list_t;

typedef struct {
	candidate_t *candidate;
	int cost;
	int edits;
	int start;
	int span;
} match_t;

typedef struct {
	int cost;
	int start;
	bool valid;
} cell_t;

typedef struct {
	cell_t *rows[3];
	size_t columns;
} matcher_t;

typedef struct {
	int cost;
	int edits;
	int start;
	int span;
} alignment_t;

typedef struct {
	char *data;
	size_t length;
	size_t capacity;
} buffer_t;

typedef struct {
	int fd;
	struct termios original;
	int drawn_rows;
} terminal_t;

typedef enum {
	KEY_CHARACTER,
	KEY_ENTER,
	KEY_CANCEL,
	KEY_BACKSPACE,
	KEY_PREVIOUS,
	KEY_NEXT,
} key_kind_t;

typedef struct {
	key_kind_t kind;
	char character;
} key_event_t;

typedef struct {
	unsigned char data[128];
	size_t length;
} input_buffer_t;

static volatile sig_atomic_t resize_pending;
static volatile sig_atomic_t stop_pending;

static int integer_abs(int value) {
	return value < 0 ? -value : value;
}

static char ascii_lower(unsigned char character) {
	if (character >= 'A' && character <= 'Z') {
		return (char)(character + ('a' - 'A'));
	}
	return (char)character;
}

static int max_edits(size_t query_length) {
	if (query_length < 3) {
		return 0;
	}
	if (query_length < 6) {
		return 1;
	}
	return 2;
}

static bool key_position(char key, int *x, int *y) {
	static const char *rows[] = {"1234567890", "qwertyuiop", "asdfghjkl", "zxcvbnm"};
	static const int offsets[] = {0, 0, 1, 2};
	for (int row = 0; row < 4; row++) {
		const char *found = strchr(rows[row], key);
		if (found != NULL) {
			*x = (int)(found - rows[row]) * 2 + offsets[row];
			*y = row * 2;
			return true;
		}
	}
	return false;
}

static int key_cost[256][256];
static bool key_cost_ready;

static void init_key_cost_table(void) {
	if (key_cost_ready) {
		return;
	}
	for (int a = 0; a < 256; a++) {
		for (int b = 0; b < 256; b++) {
			int cost;
			if (a == b) {
				cost = 0;
			} else {
				int ax, ay, bx, by;
				if (key_position((char)a, &ax, &ay) &&
				    key_position((char)b, &bx, &by) &&
				    integer_abs(ax - bx) <= 2 &&
				    integer_abs(ay - by) <= 2) {
					cost = COST_NEIGHBOR_SUBSTITUTION;
				} else {
					cost = COST_SUBSTITUTION;
				}
			}
			key_cost[a][b] = cost;
		}
	}
	key_cost_ready = true;
}

static bool add_candidate(candidate_list_t *list, const char *text) {
	if (list->length == list->capacity) {
		size_t next_capacity = list->capacity == 0 ? 64 : list->capacity * 2;
		candidate_t *next = realloc(list->items, next_capacity * sizeof(*next));
		if (next == NULL) {
			return false;
		}
		list->items = next;
		list->capacity = next_capacity;
	}

	candidate_t item;
	memset(&item, 0, sizeof(item));
	item.raw = strdup(text);
	if (item.raw == NULL) {
		return false;
	}
	item.folded = strdup(text);
	if (item.folded == NULL) {
		free(item.raw);
		return false;
	}
	item.length = strlen(text);
	for (size_t i = 0; i < item.length; i++) {
		item.folded[i] = ascii_lower((unsigned char)item.folded[i]);
	}
	list->items[list->length++] = item;
	return true;
}

static void destroy_candidates(candidate_list_t *list) {
	for (size_t i = 0; i < list->length; i++) {
		free(list->items[i].raw);
		free(list->items[i].folded);
	}
	free(list->items);
	memset(list, 0, sizeof(*list));
}

static bool init_matcher(matcher_t *matcher, size_t max_candidate_length) {
	matcher->columns = (max_candidate_length + 1) * EDIT_SLOTS;
	matcher->rows[0] = calloc(matcher->columns * 3, sizeof(*matcher->rows[0]));
	if (matcher->rows[0] == NULL) {
		return false;
	}
	matcher->rows[1] = matcher->rows[0] + matcher->columns;
	matcher->rows[2] = matcher->rows[1] + matcher->columns;
	return true;
}

static void destroy_matcher(matcher_t *matcher) {
	free(matcher->rows[0]);
}

static size_t cell_index(size_t column, int edits) {
	return column * EDIT_SLOTS + (size_t)edits;
}

static void update_cell(cell_t *cell, int cost, int start) {
	if (!cell->valid || cost < cell->cost || (cost == cell->cost && start < cell->start)) {
		cell->valid = true;
		cell->cost = cost;
		cell->start = start;
	}
}

static alignment_t align(matcher_t *matcher, const char *query, size_t query_length,
	                     const char *text, size_t text_length) {
	alignment_t best = {.cost = INT_MAX, .edits = INT_MAX,
	                    .start = INT_MAX, .span = INT_MAX};
	if (query_length == 0) {
		best.cost = 0;
		best.edits = 0;
		best.start = 0;
		best.span = 0;
		return best;
	}

	int limit = max_edits(query_length);
	cell_t *previous_previous = matcher->rows[0];
	cell_t *previous = matcher->rows[1];
	cell_t *current = matcher->rows[2];
	memset(previous_previous, 0, matcher->columns * sizeof(*previous_previous));
	memset(previous, 0, matcher->columns * sizeof(*previous));
	for (size_t column = 0; column <= text_length; column++) {
		previous[cell_index(column, 0)] = (cell_t){.cost = 0, .start = (int)column, .valid = true};
	}

	for (size_t row = 1; row <= query_length; row++) {
		memset(current, 0, matcher->columns * sizeof(*current));
		for (size_t column = 0; column <= text_length; column++) {
			for (int edits = 0; edits <= limit; edits++) {
				cell_t *destination = &current[cell_index(column, edits)];

				if (edits > 0) {
					cell_t source = previous[cell_index(column, edits - 1)];
					if (source.valid) {
						update_cell(destination, source.cost + COST_DELETION, source.start);
					}
				}

				if (column > 0) {
					int cost = key_cost[(unsigned char)query[row - 1]]
					                   [(unsigned char)text[column - 1]];
					int edit_delta = cost == 0 ? 0 : 1;
					if (edits >= edit_delta) {
						cell_t source = previous[cell_index(column - 1, edits - edit_delta)];
						if (source.valid) {
							update_cell(destination, source.cost + cost, source.start);
						}
					}
				}

				if (column > 0 && edits > 0) {
					cell_t source = current[cell_index(column - 1, edits - 1)];
					if (source.valid) {
						update_cell(destination, source.cost + COST_INSERTION, source.start);
					}
				}

				if (row > 1 && column > 1 && edits > 0 &&
				    query[row - 2] == text[column - 1] &&
				    query[row - 1] == text[column - 2]) {
					cell_t source = previous_previous[cell_index(column - 2, edits - 1)];
					if (source.valid) {
						update_cell(destination, source.cost + COST_TRANSPOSITION, source.start);
					}
				}
			}
		}
		cell_t *temporary = previous_previous;
		previous_previous = previous;
		previous = current;
		current = temporary;
	}

	for (size_t end = 0; end <= text_length; end++) {
		for (int edits = 0; edits <= limit; edits++) {
			cell_t result = previous[cell_index(end, edits)];
			if (!result.valid) {
				continue;
			}
			int span = (int)end - result.start;
			if (result.cost < best.cost ||
			    (result.cost == best.cost && edits < best.edits) ||
			    (result.cost == best.cost && edits == best.edits && result.start < best.start) ||
			    (result.cost == best.cost && edits == best.edits && result.start == best.start &&
			     span < best.span)) {
				best.cost = result.cost;
				best.edits = edits;
				best.start = result.start;
				best.span = span;
			}
		}
	}
	return best;
}

static int compare_matches(const void *left, const void *right) {
	const match_t *a = left;
	const match_t *b = right;
	int rank_a = a->cost + (int)a->candidate->length - a->span;
	int rank_b = b->cost + (int)b->candidate->length - b->span;
	if (a->edits != b->edits) {
		return a->edits < b->edits ? -1 : 1;
	}
	if (rank_a != rank_b) {
		return rank_a < rank_b ? -1 : 1;
	}
	if (a->start != b->start) {
		return a->start < b->start ? -1 : 1;
	}
	if (a->cost != b->cost) {
		return a->cost < b->cost ? -1 : 1;
	}
	if (a->candidate->length != b->candidate->length) {
		return a->candidate->length < b->candidate->length ? -1 : 1;
	}
	if (a->candidate == b->candidate) {
		return 0;
	}
	return a->candidate < b->candidate ? -1 : 1;
}

static size_t find_matches(matcher_t *matcher, candidate_list_t *candidates,
	                       const char *query, size_t query_length, match_t *matches) {
	init_key_cost_table();

	if (query_length == 0) {
		for (size_t i = 0; i < candidates->length; i++) {
			matches[i] = (match_t){.candidate = &candidates->items[i]};
		}
		return candidates->length;
	}

	int limit = (query_length == 0) ? 0 : max_edits(query_length);
	size_t minimum_length = 0;
	if (query_length > (size_t)limit) {
		minimum_length = query_length - (size_t)limit;
	}
	int required_exact = (int)query_length - limit;

	int query_counts[256];
	memset(query_counts, 0, sizeof(query_counts));
	if (query_length > 0) {
		for (size_t i = 0; i < query_length; i++) {
			query_counts[(unsigned char)query[i]]++;
		}
	}

	size_t count = 0;
	for (size_t i = 0; i < candidates->length; i++) {
		candidate_t *candidate = &candidates->items[i];
		if (query_length > 0) {
			if (candidate->length < minimum_length) {
				continue;
			}
			if (required_exact > 0) {
				int remaining[256];
				memcpy(remaining, query_counts, sizeof(remaining));
				int common = 0;
				for (size_t ci = 0;
				     ci < candidate->length && common < required_exact;
				     ci++) {
					unsigned char character =
					    (unsigned char)candidate->folded[ci];
					if (remaining[character] > 0) {
						remaining[character]--;
						common++;
					}
				}
				if (common < required_exact) {
					continue;
				}
			}
		}
		alignment_t result;
		if (query_length == 1) {
			const char *found = strchr(candidate->folded, query[0]);
			if (found == NULL) {
				continue;
			}
			result.cost = 0;
			result.edits = 0;
			result.start = (int)(found - candidate->folded);
			result.span = 1;
		} else if (query_length == 2) {
			int position = -1;
			for (size_t ci = 0; ci + 1 < candidate->length; ci++) {
				if (candidate->folded[ci] == query[0] &&
				    candidate->folded[ci + 1] == query[1]) {
					position = (int)ci;
					break;
				}
			}
			if (position < 0) {
				continue;
			}
			result.cost = 0;
			result.edits = 0;
			result.start = position;
			result.span = 2;
		} else {
			result = align(matcher, query, query_length,
			               candidate->folded, candidate->length);
		}
		if (result.cost == INT_MAX) {
			continue;
		}
		matches[count++] = (match_t){
			.candidate = candidate,
			.cost = result.cost,
			.edits = result.edits,
			.start = result.start,
			.span = result.span,
		};
	}
	qsort(matches, count, sizeof(*matches), compare_matches);
	return count;
}

static bool reserve_buffer(buffer_t *buffer, size_t required) {
	if (required <= buffer->capacity) {
		return true;
	}
	size_t capacity = buffer->capacity == 0 ? 4096 : buffer->capacity;
	while (capacity < required) {
		capacity *= 2;
	}
	char *next = realloc(buffer->data, capacity);
	if (next == NULL) {
		return false;
	}
	buffer->data = next;
	buffer->capacity = capacity;
	return true;
}

static bool append_bytes(buffer_t *buffer, const char *text, size_t length) {
	if (!reserve_buffer(buffer, buffer->length + length + 1)) {
		return false;
	}
	memcpy(buffer->data + buffer->length, text, length);
	buffer->length += length;
	buffer->data[buffer->length] = '\0';
	return true;
}

static bool append_string(buffer_t *buffer, const char *text) {
	return append_bytes(buffer, text, strlen(text));
}

static bool append_cursor_move(buffer_t *buffer, int amount, char direction) {
	if (amount <= 0) {
		return true;
	}
	char sequence[32];
	int length = snprintf(sequence, sizeof(sequence), "\033[%d%c", amount, direction);
	return length > 0 && (size_t)length < sizeof(sequence) &&
	       append_bytes(buffer, sequence, (size_t)length);
}

static bool append_display(buffer_t *buffer, const char *text, int limit, int *used) {
	for (size_t i = 0; text[i] != '\0'; i++) {
		unsigned char character = (unsigned char)text[i];
		if (*used >= limit) {
			break;
		}
		if (character < 0x20 || character > 0x7e) {
			character = '?';
		}
		char output = (char)character;
		if (!append_bytes(buffer, &output, 1)) {
			return false;
		}
		(*used)++;
	}
	return true;
}

static bool render_menu(buffer_t *buffer, int rows, int columns, int *drawn_rows,
	                    const char *query,
	                    match_t *matches, size_t match_count,
	                    size_t selected, size_t *top) {
	if (rows < 1) {
		rows = 1;
	}
	if (columns < 1) {
		columns = 1;
	}
	int menu_rows = 1;
	if (rows > 1) {
		size_t result_rows = match_count;
		if (result_rows > (size_t)(rows - 1)) {
			result_rows = (size_t)(rows - 1);
		}
		menu_rows += (int)result_rows;
	}
	int paint_rows = menu_rows > *drawn_rows ? menu_rows : *drawn_rows;
	if (paint_rows > rows) {
		paint_rows = rows;
	}
	size_t visible = (size_t)(menu_rows - 1);
	if (visible > 0) {
		if (selected < *top) {
			*top = selected;
		}
		if (selected >= *top + visible) {
			*top = selected - visible + 1;
		}
	}
	int query_width = 0;

	buffer->length = 0;
	if (!append_string(buffer, "\r")) {
		return false;
	}
	for (int row = 0; row < paint_rows; row++) {
		if (!append_string(buffer, "\033[2K")) {
			return false;
		}
		if (row == 0) {
			if (!append_string(buffer, "\033[1m") ||
			    !append_display(buffer, query, columns, &query_width) ||
			    !append_string(buffer, "\033[0m")) {
				return false;
			}
		} else if (row < menu_rows) {
			size_t index = *top + (size_t)row - 1;
			if (index < match_count) {
				if (index == selected && !append_string(buffer, "\033[7m")) {
					return false;
				}
				int used = 0;
				if (!append_display(buffer, matches[index].candidate->raw, columns, &used)) {
					return false;
				}
				if (index == selected && !append_string(buffer, "\033[0m")) {
					return false;
				}
			}
		}
		if (row + 1 < paint_rows && !append_string(buffer, "\r\n")) {
			return false;
		}
	}
	if (!append_cursor_move(buffer, paint_rows - 1, 'A')) {
		return false;
	}
	if (!append_string(buffer, "\r")) {
		return false;
	}
	if (!append_cursor_move(buffer, query_width, 'C')) {
		return false;
	}
	*drawn_rows = menu_rows;
	return true;
}

static bool write_all(int fd, const char *data, size_t length) {
	while (length > 0) {
		ssize_t written = write(fd, data, length);
		if (written < 0) {
			if (errno == EINTR) {
				continue;
			}
			return false;
		}
		data += written;
		length -= (size_t)written;
	}
	return true;
}

static bool terminal_size(int fd, int *rows, int *columns) {
	struct winsize size;
	if (ioctl(fd, TIOCGWINSZ, &size) < 0) {
		return false;
	}
	*rows = size.ws_row == 0 ? 24 : size.ws_row;
	*columns = size.ws_col == 0 ? 80 : size.ws_col;
	return true;
}

static bool start_terminal(terminal_t *terminal) {
	memset(terminal, 0, sizeof(*terminal));
	terminal->fd = open("/dev/tty", O_RDWR);
	if (terminal->fd < 0) {
		return false;
	}
	if (tcgetattr(terminal->fd, &terminal->original) < 0) {
		close(terminal->fd);
		terminal->fd = -1;
		return false;
	}
	struct termios raw = terminal->original;
	raw.c_iflag &= (tcflag_t)~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
	raw.c_oflag &= (tcflag_t)~OPOST;
	raw.c_cflag |= CS8;
	raw.c_lflag &= (tcflag_t)~(ECHO | ICANON | IEXTEN | ISIG);
	raw.c_cc[VMIN] = 0;
	raw.c_cc[VTIME] = 1;
	if (tcsetattr(terminal->fd, TCSAFLUSH, &raw) < 0) {
		close(terminal->fd);
		terminal->fd = -1;
		return false;
	}
	terminal->drawn_rows = 1;
	return true;
}

static void close_terminal(terminal_t *terminal) {
	int clear_rows = terminal->drawn_rows;
	int rows, columns;
	if (terminal_size(terminal->fd, &rows, &columns) && clear_rows > rows) {
		clear_rows = rows;
	}
	char movement[32];
	(void)write_all(terminal->fd, "\r", 1);
	for (int row = 0; row < clear_rows; row++) {
		(void)write_all(terminal->fd, "\033[2K", 4);
		if (row + 1 < clear_rows) {
			(void)write_all(terminal->fd, "\r\n", 2);
		}
	}
	if (clear_rows > 1) {
		int length = snprintf(movement, sizeof(movement), "\033[%dA", clear_rows - 1);
		if (length > 0 && (size_t)length < sizeof(movement)) {
			(void)write_all(terminal->fd, movement, (size_t)length);
		}
	}
	(void)write_all(terminal->fd, "\r\033[0m", 5);
	(void)tcsetattr(terminal->fd, TCSAFLUSH, &terminal->original);
	(void)close(terminal->fd);
}

static void consume_input(input_buffer_t *input, size_t count) {
	memmove(input->data, input->data + count, input->length - count);
	input->length -= count;
}

static bool next_key(input_buffer_t *input, bool timed_out, key_event_t *event) {
	event->character = '\0';
	while (input->length > 0) {
		unsigned char *data = input->data;
		if (data[0] == 0x1b) {
			size_t count = 1;
			if (input->length == 1) {
				if (!timed_out) {
					return false;
				}
			} else if (data[1] == '[' || data[1] == 'O') {
				size_t end = 2;
				while (end < input->length && (data[end] < 0x40 || data[end] > 0x7e)) {
					end++;
				}
				if (end == input->length && !timed_out) {
					return false;
				}
				count = end < input->length ? end + 1 : input->length;
		}
			consume_input(input, count);
			continue;
		}

		unsigned char character = data[0];
		consume_input(input, 1);
		switch (character) {
		case 3:
			event->kind = KEY_CANCEL;
			return true;
		case '\r':
			event->kind = KEY_ENTER;
			return true;
		case '\n':
			event->kind = KEY_NEXT;
			return true;
		case 11:
			event->kind = KEY_PREVIOUS;
			return true;
		case 127:
			event->kind = KEY_BACKSPACE;
			return true;
		default:
			break;
		}
		if (character >= 0x20 && character <= 0x7e) {
			event->kind = KEY_CHARACTER;
			event->character = (char)character;
			return true;
		}
	}
	return false;
}

static void handle_resize(int signal_number) {
	(void)signal_number;
	resize_pending = 1;
}

static void handle_stop(int signal_number) {
	(void)signal_number;
	stop_pending = 1;
}

static bool install_signals(void) {
	struct sigaction resize_action;
	memset(&resize_action, 0, sizeof(resize_action));
	resize_action.sa_handler = handle_resize;
	sigemptyset(&resize_action.sa_mask);
	if (sigaction(SIGWINCH, &resize_action, NULL) < 0) {
		return false;
	}
	struct sigaction stop_action;
	memset(&stop_action, 0, sizeof(stop_action));
	stop_action.sa_handler = handle_stop;
	sigemptyset(&stop_action.sa_mask);
	return sigaction(SIGINT, &stop_action, NULL) == 0 &&
	       sigaction(SIGTERM, &stop_action, NULL) == 0;
}

static bool load_candidates(candidate_list_t *list, int argc, char **argv) {
	if (argc > 1) {
		for (int i = 1; i < argc; i++) {
			if (!add_candidate(list, argv[i])) {
				return false;
			}
		}
		return true;
	}

	char *line = NULL;
	size_t capacity = 0;
	ssize_t length;
	while ((length = getline(&line, &capacity, stdin)) >= 0) {
		while (length > 0 && (line[length - 1] == '\n' || line[length - 1] == '\r')) {
			line[--length] = '\0';
		}
		if (!add_candidate(list, line)) {
			free(line);
			return false;
		}
	}
	free(line);
	return !ferror(stdin);
}

static int run_menu(candidate_list_t *candidates, char *query,
	                size_t query_length, matcher_t *matcher) {
	match_t *matches = calloc(candidates->length == 0 ? 1 : candidates->length,
	                          sizeof(*matches));
	if (matches == NULL) {
		fprintf(stderr, "fuzzymenu: out of memory\n");
		return 2;
	}
	size_t match_count = find_matches(matcher, candidates, query, query_length, matches);
	size_t selected = 0;
	size_t top = 0;

	terminal_t terminal;
	if (!start_terminal(&terminal)) {
		fprintf(stderr, "fuzzymenu: cannot initialize /dev/tty: %s\n", strerror(errno));
		free(matches);
		return 2;
	}
	if (!install_signals()) {
		close_terminal(&terminal);
		fprintf(stderr, "fuzzymenu: cannot install signal handlers: %s\n", strerror(errno));
		free(matches);
		return 2;
	}

	buffer_t frame = {0};
	input_buffer_t input = {0};
	const char *selection = NULL;
	bool redraw = true;
	int result = 1;
	for (;;) {
		if (stop_pending) {
			result = 1;
			break;
		}
		if (resize_pending) {
			resize_pending = 0;
			redraw = true;
		}
		if (redraw) {
			int rows, columns;
			if (!terminal_size(terminal.fd, &rows, &columns)) {
				fprintf(stderr, "fuzzymenu: cannot draw terminal: %s\n", strerror(errno));
				result = 2;
				break;
			}
			if (!render_menu(&frame, rows, columns, &terminal.drawn_rows, query,
			                 matches, match_count, selected, &top) ||
			    !write_all(terminal.fd, frame.data, frame.length)) {
				fprintf(stderr, "fuzzymenu: cannot draw terminal: %s\n", strerror(errno));
				result = 2;
				break;
			}
			redraw = false;
		}

		unsigned char bytes[64];
		ssize_t length = read(terminal.fd, bytes, sizeof(bytes));
		if (length < 0) {
			if (errno == EINTR) {
				continue;
			}
			fprintf(stderr, "fuzzymenu: cannot read terminal: %s\n", strerror(errno));
			result = 2;
			break;
		}
		if (length > 0) {
			size_t available = sizeof(input.data) - input.length;
			if ((size_t)length > available) {
				input.length = 0;
				available = sizeof(input.data);
			}
			size_t copy = (size_t)length < available ? (size_t)length : available;
			memcpy(input.data + input.length, bytes, copy);
			input.length += copy;
		}

		bool timed_out = length == 0;
		key_event_t event;
		while (next_key(&input, timed_out, &event)) {
			bool query_changed = false;
			switch (event.kind) {
			case KEY_CANCEL:
				result = 1;
				goto done;
			case KEY_ENTER:
				if (match_count > 0) {
					selection = matches[selected].candidate->raw;
					result = 0;
					goto done;
				}
				break;
			case KEY_BACKSPACE:
				if (query_length > 0) {
					query[--query_length] = '\0';
					query_changed = true;
				}
				break;
			case KEY_CHARACTER:
				if (query_length < MAX_QUERY) {
					query[query_length++] = ascii_lower((unsigned char)event.character);
					query[query_length] = '\0';
					query_changed = true;
				}
				break;
			case KEY_PREVIOUS:
				if (selected > 0) selected--;
				break;
			case KEY_NEXT:
				if (selected + 1 < match_count) selected++;
				break;
			}

			if (query_changed) {
				match_count = find_matches(matcher, candidates, query, query_length, matches);
				selected = 0;
				top = 0;
			}
			redraw = true;
		}
	}

done:
	close_terminal(&terminal);
	if (selection != NULL) {
		printf("%s\n", selection);
	}
	free(frame.data);
	free(matches);
	return result;
}

int main(int argc, char **argv) {
	candidate_list_t candidates = {0};
	if (!load_candidates(&candidates, argc, argv)) {
		fprintf(stderr, "fuzzymenu: cannot read candidates: %s\n", strerror(errno));
		destroy_candidates(&candidates);
		return 2;
	}
	size_t max_candidate_length = 0;
	for (size_t i = 0; i < candidates.length; i++) {
		if (candidates.items[i].length > max_candidate_length) {
			max_candidate_length = candidates.items[i].length;
		}
	}

	matcher_t matcher = {0};
	if (!init_matcher(&matcher, max_candidate_length)) {
		fprintf(stderr, "fuzzymenu: out of memory\n");
		destroy_candidates(&candidates);
		return 2;
	}

	char query[MAX_QUERY + 1] = {0};
	int result = run_menu(&candidates, query, 0, &matcher);
	destroy_matcher(&matcher);
	destroy_candidates(&candidates);
	return result;
}
