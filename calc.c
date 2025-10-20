// Nandana Subhash 241ADB029
// Compile with: gcc -O2 -Wall -Wextra -std=c17 -o calc calc.c -lm

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <errno.h>
#include <dirent.h>
#include <sys/stat.h>

// Token types for our parser
typedef enum {
    TOK_NUMBER,
    TOK_PLUS,
    TOK_MINUS,
    TOK_STAR,
    TOK_SLASH,
    TOK_POW,
    TOK_LPAREN,
    TOK_RPAREN,
    TOK_EOF,
    TOK_ERROR
} TokenType;

// Token structure with value and position
typedef struct {
    TokenType type;
    double value;       // For numbers
    int start_pos;      // 1-based character position
} Token;

// Parser state
typedef struct {
    const char *input;      // Input string
    int length;             // Input length
    int position;           // Current read position
    Token current_token;    // Current token
    int has_error;          // Error flag
    int error_position;     // Position of first error
} Parser;

// Global variables for CLI options
char *input_dir = NULL;
char *output_dir = NULL;
char *single_file = NULL;

// Function declarations
void parse_args(int argc, char *argv[]);
void process_single_file(const char *input_path);
void process_directory(const char *dir_path);
char* build_output_path(const char *input_path);
double evaluate_expression(Parser *parser);
double parse_expression(Parser *parser);
double parse_term(Parser *parser);
double parse_factor(Parser *parser);
double parse_power(Parser *parser);
double parse_primary(Parser *parser);
void next_token(Parser *parser);
void fail(Parser *parser, int pos);
int is_whitespace(char c);
int should_skip_line(const char *input, int *pos, int length);

// Initialize parser with input string
void init_parser(Parser *parser, const char *input, int length) {
    parser->input = input;
    parser->length = length;
    parser->position = 0;
    parser->has_error = 0;
    parser->error_position = -1;
    next_token(parser); // Load first token
}

// Report error at specific position
void fail(Parser *parser, int pos) {
    if (!parser->has_error) {
        parser->has_error = 1;
        parser->error_position = pos;
    }
}

// Check if character is whitespace
int is_whitespace(char c) {
    return c == ' ' || c == '\t' || c == '\r' || c == '\n';
}

// Skip whitespace and comments, return 1 if we should skip the entire line
int should_skip_line(const char *input, int *pos, int length) {
    int original_pos = *pos;
    
    // Skip whitespace
    while (*pos < length && is_whitespace(input[*pos])) {
        (*pos)++;
    }
    
    // Check if line starts with comment
    if (*pos < length && input[*pos] == '#') {
        // Skip entire line
        while (*pos < length && input[*pos] != '\n') {
            (*pos)++;
        }
        // Skip the newline itself
        if (*pos < length && input[*pos] == '\n') {
            (*pos)++;
        }
        return 1;
    }
    
    // If we moved but didn't find a comment, restore position
    if (*pos != original_pos && !(*pos < length && input[*pos] == '#')) {
        *pos = original_pos;
    }
    
    return 0;
}

// Get next token from input
void next_token(Parser *parser) {
    // Skip whitespace and comments
    while (parser->position < parser->length) {
        if (should_skip_line(parser->input, &parser->position, parser->length)) {
            continue;
        }
        if (is_whitespace(parser->input[parser->position])) {
            parser->position++;
        } else {
            break;
        }
    }
    
    // Check for end of input
    if (parser->position >= parser->length) {
        parser->current_token.type = TOK_EOF;
        parser->current_token.start_pos = parser->position + 1;
        return;
    }
    
    char current_char = parser->input[parser->position];
    int start_pos = parser->position + 1; // 1-based position
    
    // Number token (integer or float)
    if (isdigit(current_char) || current_char == '.' || 
        ((current_char == '+' || current_char == '-') && 
         parser->position + 1 < parser->length && 
         (isdigit(parser->input[parser->position + 1]) || 
          parser->input[parser->position + 1] == '.'))) {
        
        char *end_ptr;
        double value = strtod(parser->input + parser->position, &end_ptr);
        
        if (end_ptr == parser->input + parser->position) {
            // No number parsed - this shouldn't happen with our checks
            parser->current_token.type = TOK_ERROR;
            parser->current_token.start_pos = start_pos;
            fail(parser, start_pos);
            return;
        }
        
        parser->current_token.type = TOK_NUMBER;
        parser->current_token.value = value;
        parser->current_token.start_pos = start_pos;
        parser->position = end_ptr - parser->input;
        return;
    }
    
    // Single character tokens
    parser->current_token.start_pos = start_pos;
    
    switch (current_char) {
        case '+':
            parser->current_token.type = TOK_PLUS;
            parser->position++;
            break;
        case '-':
            parser->current_token.type = TOK_MINUS;
            parser->position++;
            break;
        case '*':
            if (parser->position + 1 < parser->length && 
                parser->input[parser->position + 1] == '*') {
                parser->current_token.type = TOK_POW;
                parser->position += 2;
            } else {
                parser->current_token.type = TOK_STAR;
                parser->position++;
            }
            break;
        case '/':
            parser->current_token.type = TOK_SLASH;
            parser->position++;
            break;
        case '(':
            parser->current_token.type = TOK_LPAREN;
            parser->position++;
            break;
        case ')':
            parser->current_token.type = TOK_RPAREN;
            parser->position++;
            break;
        default:
            parser->current_token.type = TOK_ERROR;
            fail(parser, start_pos);
            break;
    }
}

// Parse primary expressions: numbers or parenthesized expressions
double parse_primary(Parser *parser) {
    if (parser->has_error) return 0;
    
    Token token = parser->current_token;
    
    if (token.type == TOK_NUMBER) {
        next_token(parser);
        return token.value;
    }
    else if (token.type == TOK_LPAREN) {
        next_token(parser); // consume '('
        double result = parse_expression(parser);
        
        if (parser->has_error) return 0;
        
        if (parser->current_token.type != TOK_RPAREN) {
            fail(parser, parser->current_token.start_pos);
            return 0;
        }
        
        next_token(parser); // consume ')'
        return result;
    }
    else {
        fail(parser, token.start_pos);
        return 0;
    }
}

// Parse power operator (right-associative)
double parse_power(Parser *parser) {
    if (parser->has_error) return 0;
    
    double left = parse_primary(parser);
    
    while (!parser->has_error && parser->current_token.type == TOK_POW) {
        int op_pos = parser->current_token.start_pos;
        next_token(parser);
        
        double right = parse_power(parser); // Right-associative: recurse for right side
        
        if (parser->has_error) return 0;
        
        // Check for mathematical errors
        if (left == 0 && right < 0) {
            fail(parser, op_pos); // 0 raised to negative power
            return 0;
        }
        if (left < 0 && fmod(right, 1.0) != 0.0) {
            fail(parser, op_pos); // Negative base with fractional exponent
            return 0;
        }
        
        left = pow(left, right);
    }
    
    return left;
}

// Parse factor (currently same as power, reserved for unary operators)
double parse_factor(Parser *parser) {
    return parse_power(parser);
}

// Parse term: multiplication and division (left-associative)
double parse_term(Parser *parser) {
    if (parser->has_error) return 0;
    
    double result = parse_factor(parser);
    
    while (!parser->has_error && (parser->current_token.type == TOK_STAR || 
                                 parser->current_token.type == TOK_SLASH)) {
        Token op = parser->current_token;
        next_token(parser);
        
        double right = parse_factor(parser);
        
        if (parser->has_error) return 0;
        
        if (op.type == TOK_STAR) {
            result *= right;
        }
        else { // TOK_SLASH
            if (right == 0.0) {
                fail(parser, op.start_pos); // Division by zero
                return 0;
            }
            result /= right;
        }
    }
    
    return result;
}

// Parse expression: addition and subtraction (left-associative)
double parse_expression(Parser *parser) {
    if (parser->has_error) return 0;
    
    double result = parse_term(parser);
    
    while (!parser->has_error && (parser->current_token.type == TOK_PLUS || 
                                 parser->current_token.type == TOK_MINUS)) {
        Token op = parser->current_token;
        next_token(parser);
        
        double right = parse_term(parser);
        
        if (parser->has_error) return 0;
        
        if (op.type == TOK_PLUS) {
            result += right;
        }
        else { // TOK_MINUS
            result -= right;
        }
    }
    
    return result;
}

// Main evaluation function
double evaluate_expression(Parser *parser) {
    double result = parse_expression(parser);
    
    // Check if we have unparsed tokens at the end
    if (!parser->has_error && parser->current_token.type != TOK_EOF) {
        fail(parser, parser->current_token.start_pos);
        return 0;
    }
    
    return result;
}

// Parse command line arguments
void parse_args(int argc, char *argv[]) {
    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-d") == 0 || strcmp(argv[i], "--dir") == 0) {
            if (i + 1 < argc) {
                input_dir = argv[++i];
            } else {
                fprintf(stderr, "Error: %s requires a directory argument\n", argv[i]);
                exit(1);
            }
        }
        else if (strcmp(argv[i], "-o") == 0 || strcmp(argv[i], "--output-dir") == 0) {
            if (i + 1 < argc) {
                output_dir = argv[++i];
            } else {
                fprintf(stderr, "Error: %s requires a directory argument\n", argv[i]);
                exit(1);
            }
        }
        else if (argv[i][0] == '-') {
            fprintf(stderr, "Error: Unknown option %s\n", argv[i]);
            exit(1);
        }
        else {
            single_file = argv[i];
        }
    }
    
    if (input_dir && single_file) {
        fprintf(stderr, "Error: Cannot specify both directory and single file\n");
        exit(1);
    }
    
    if (!input_dir && !single_file) {
        fprintf(stderr, "Error: No input file or directory specified\n");
        exit(1);
    }
}

// Create directory if it doesn't exist
void create_directory_if_missing(const char *path) {
    struct stat st;
    if (stat(path, &st) != 0) {
        // Directory doesn't exist, create it
        if (mkdir(path, 0775) != 0) {
            perror("Error creating directory");
            exit(1);
        }
    } else if (!S_ISDIR(st.st_mode)) {
        fprintf(stderr, "Error: %s exists but is not a directory\n", path);
        exit(1);
    }
}

// Build output file path
char* build_output_path(const char *input_path) {
    static char output_path[1024];
    const char *filename = strrchr(input_path, '/');
    if (filename == NULL) {
        filename = input_path;
    } else {
        filename++; // Skip the '/'
    }
    
    // Extract base name (without extension)
    char base_name[256];
    strcpy(base_name, filename);
    char *dot = strrchr(base_name, '.');
    if (dot != NULL) {
        *dot = '\0';
    }
    
    if (output_dir) {
        snprintf(output_path, sizeof(output_path), "%s/%s_john_doe_123456.txt", 
                 output_dir, base_name);
    } else {
        // Create default output directory name
        char default_dir[256];
        snprintf(default_dir, sizeof(default_dir), "%s_john_doe_123456", base_name);
        create_directory_if_missing(default_dir);
        snprintf(output_path, sizeof(output_path), "%s/%s_john_doe_123456.txt", 
                 default_dir, base_name);
    }
    
    return output_path;
}

// Process a single input file
void process_single_file(const char *input_path) {
    FILE *file = fopen(input_path, "r");
    if (!file) {
        perror("Error opening input file");
        exit(1);
    }
    
    // Read entire file
    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);
    
    char *input = malloc(file_size + 1);
    if (!input) {
        perror("Memory allocation error");
        fclose(file);
        exit(1);
    }
    
    size_t bytes_read = fread(input, 1, file_size, file);
    input[bytes_read] = '\0';
    fclose(file);
    
    // Parse and evaluate
    Parser parser;
    init_parser(&parser, input, bytes_read);
    double result = evaluate_expression(&parser);
    
    // Write output
    char *output_path = build_output_path(input_path);
    FILE *output_file = fopen(output_path, "w");
    if (!output_file) {
        perror("Error opening output file");
        free(input);
        exit(1);
    }
    
    if (parser.has_error) {
        fprintf(output_file, "ERROR:%d\n", parser.error_position);
    } else {
        // Check if result is integer
        double int_part;
        if (fabs(result - floor(result)) < 1e-12 || fabs(result - ceil(result)) < 1e-12) {
            fprintf(output_file, "%.0f\n", result);
        } else {
            fprintf(output_file, "%.15g\n", result);
        }
    }
    
    fclose(output_file);
    free(input);
}

// Process all .txt files in a directory
void process_directory(const char *dir_path) {
    DIR *dir = opendir(dir_path);
    if (!dir) {
        perror("Error opening directory");
        exit(1);
    }
    
    // Create output directory if specified
    if (output_dir) {
        create_directory_if_missing(output_dir);
    }
    
    struct dirent *entry;
    while ((entry = readdir(dir)) != NULL) {
        // Check if file ends with .txt
        const char *name = entry->d_name;
        size_t len = strlen(name);
        if (len > 4 && strcmp(name + len - 4, ".txt") == 0) {
            // Build full path
            char full_path[1024];
            snprintf(full_path, sizeof(full_path), "%s/%s", dir_path, name);
            
            printf("Processing: %s\n", name);
            process_single_file(full_path);
        }
    }
    
    closedir(dir);
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s [-d DIR | --dir DIR] [-o OUTDIR | --output-dir OUTDIR] [input.txt]\n", argv[0]);
        return 1;
    }
    
    parse_args(argc, argv);
    
    if (output_dir) {
        create_directory_if_missing(output_dir);
    }
    
    if (input_dir) {
        process_directory(input_dir);
    } else {
        process_single_file(single_file);
    }
    
    return 0;
}
