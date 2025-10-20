// Nandana Subhash 241ADB029
// https://github.com/mli1323/C-programming
// Compile with: gcc -O2 -Wall -Wextra -std=c17 -o calc calc.c -lm

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <errno.h>
#include <dirent.h>
#include <sys/stat.h>
#include <stdarg.h>

#define MAX_INPUT_SIZE 10000
#define MAX_FILENAME 256
#define MAX_OUTPUT_PATH 512
#define MAX_FOLDER_NAME 512

// Token types for lexical analysis
typedef enum {
    TK_INTEGER,
    TK_FLOAT,
    TK_ADD,
    TK_SUBTRACT,
    TK_MULTIPLY,
    TK_DIVIDE,
    TK_POWER,
    TK_LEFT_PAREN,
    TK_RIGHT_PAREN,
    TK_END,
    TK_BAD
} TokenKind;

// Token structure stores type, value, and position for error reporting
typedef struct {
    TokenKind kind;
    double numeric_value;
    int char_index;     // 1-based position for error messages
    int token_length;
} Token;

// Scanner state for tokenizing input
typedef struct {
    const char* source_text;
    int text_length;
    int current_index;
    Token lookahead;
    int has_error;
    int error_position;
} Scanner;

// Parser state with error tracking
typedef struct {
    Scanner* scanner;
    int failed;
    int fail_position;
    const char* fail_reason;
} ExpressionParser;

// Configuration for command line options
typedef struct {
    char input_path[MAX_FILENAME];
    char output_path[MAX_FILENAME];
    int directory_mode;
} ProgramOptions;

// Initialize scanner with input text
static void setup_scanner(Scanner* scan, const char* text, int len) {
    scan->source_text = text;
    scan->text_length = len;
    scan->current_index = 0;
    scan->has_error = 0;
    scan->error_position = 0;
    scan->lookahead.kind = TK_BAD;
    scan->lookahead.char_index = 1;
}

// Skip whitespace to find next token
static void skip_spaces(Scanner* scan) {
    while (scan->current_index < scan->text_length && 
           isspace(scan->source_text[scan->current_index])) {
        scan->current_index++;
    }
}

// Convert input characters to tokens with position tracking
static Token fetch_next_token(Scanner* scan) {
    skip_spaces(scan);
    
    // Return END token when input is exhausted
    if (scan->current_index >= scan->text_length) {
        return (Token){TK_END, 0.0, scan->current_index + 1, 0};
    }
    
    int start_index = scan->current_index;
    int char_position = start_index + 1; // Convert to 1-based indexing
    char current_char = scan->source_text[start_index];
    
    // Handle single-character operators and parentheses
    switch (current_char) {
        case '+': scan->current_index++; return (Token){TK_ADD, 0.0, char_position, 1};
        case '-': scan->current_index++; return (Token){TK_SUBTRACT, 0.0, char_position, 1};
        case '*': 
            // Check for ** operator (exponentiation)
            if (scan->current_index + 1 < scan->text_length && 
                scan->source_text[scan->current_index + 1] == '*') {
                scan->current_index += 2;
                return (Token){TK_POWER, 0.0, char_position, 2};
            }
            scan->current_index++;
            return (Token){TK_MULTIPLY, 0.0, char_position, 1};
        case '/': scan->current_index++; return (Token){TK_DIVIDE, 0.0, char_position, 1};
        case '(': scan->current_index++; return (Token){TK_LEFT_PAREN, 0.0, char_position, 1};
        case ')': scan->current_index++; return (Token){TK_RIGHT_PAREN, 0.0, char_position, 1};
    }
    
    // Parse numeric literals using strtod for proper float handling
    if (isdigit(current_char) || current_char == '.') {
        char* parse_end;
        double num_value = strtod(scan->source_text + start_index, &parse_end);
        int token_len = parse_end - (scan->source_text + start_index);
        
        if (token_len > 0) {
            scan->current_index += token_len;
            // Determine if it's integer or float by checking for decimal/scientific notation
            TokenKind num_kind = TK_INTEGER;
            for (const char* p = scan->source_text + start_index; p < parse_end; p++) {
                if (*p == '.' || *p == 'e' || *p == 'E') {
                    num_kind = TK_FLOAT;
                    break;
                }
            }
            return (Token){num_kind, num_value, char_position, token_len};
        }
    }
    
    // Invalid character encountered
    scan->current_index++;
    scan->has_error = 1;
    scan->error_position = char_position;
    return (Token){TK_BAD, 0.0, char_position, 1};
}

static Token scanner_current(Scanner* scan) {
    return scan->lookahead;
}

static void scanner_advance(Scanner* scan) {
    scan->lookahead = fetch_next_token(scan);
}

static void initialize_scanner(Scanner* scan, const char* text, int len) {
    setup_scanner(scan, text, len);
    scanner_advance(scan);
}

// Record first parsing error with position and reason
static void parser_fail(ExpressionParser* parser, int where, const char* reason) {
    if (!parser->failed) {
        parser->failed = 1;
        parser->fail_position = where;
        parser->fail_reason = reason;
    }
}

static int parser_check(ExpressionParser* parser, TokenKind expected) {
    return scanner_current(parser->scanner).kind == expected;
}

static int parser_accept(ExpressionParser* parser, TokenKind expected) {
    if (parser_check(parser, expected)) {
        scanner_advance(parser->scanner);
        return 1;
    }
    return 0;
}

// Forward declarations for recursive descent parser
static double parse_primary_expression(ExpressionParser* parser);
static double parse_exponent_expression(ExpressionParser* parser);
static double parse_multiplicative_expression(ExpressionParser* parser);
static double parse_additive_expression(ExpressionParser* parser);

// primary := NUMBER | '(' expr ')'
// Handles atomic expressions - numbers and parenthesized subexpressions
static double parse_primary_expression(ExpressionParser* parser) {
    Token current = scanner_current(parser->scanner);
    
    if (parser_accept(parser, TK_INTEGER) || parser_accept(parser, TK_FLOAT)) {
        return current.numeric_value;
    }
    
    if (parser_accept(parser, TK_LEFT_PAREN)) {
        double inner_result = parse_additive_expression(parser);
        if (parser->failed) return 0.0;
        
        // Require matching closing parenthesis
        if (!parser_accept(parser, TK_RIGHT_PAREN)) {
            parser_fail(parser, scanner_current(parser->scanner).char_index, "Missing closing parenthesis");
            return 0.0;
        }
        return inner_result;
    }
    
    parser_fail(parser, current.char_index, "Expected number or opening parenthesis");
    return 0.0;
}

// power := primary { '' primary }  (right-associative)
// Exponentiation has highest precedence and is right-associative per Python rules
static double parse_exponent_expression(ExpressionParser* parser) {
    double base = parse_primary_expression(parser);
    if (parser->failed) return 0.0;
    
    while (parser_accept(parser, TK_POWER)) {
        int op_position = scanner_current(parser->scanner).char_index;
        // Right-associative: recursively parse exponent for 2*32 = 2(3*2)
        double exponent = parse_exponent_expression(parser);
        
        if (parser->failed) return 0.0;
        
        // Check for mathematical errors in exponentiation
        if (base == 0.0 && exponent < 0) {
            parser_fail(parser, op_position, "Zero raised to negative power");
            return 0.0;
        }
        
        double result = pow(base, exponent);
        if (isinf(result) || isnan(result)) {
            parser_fail(parser, op_position, "Math error in exponentiation");
            return 0.0;
        }
        base = result;
    }
    
    return base;
}

// term := power { ('*' | '/') power }
// Handle multiplication and division with left associativity
static double parse_multiplicative_expression(ExpressionParser* parser) {
    double result = parse_exponent_expression(parser);
    if (parser->failed) return 0.0;
    
    while (1) {
        Token op = scanner_current(parser->scanner);
        
        if (parser_accept(parser, TK_MULTIPLY)) {
            double right = parse_exponent_expression(parser);
            if (parser->failed) return 0.0;
            result *= right;
        }
        else if (parser_accept(parser, TK_DIVIDE)) {
            double right = parse_exponent_expression(parser);
            if (parser->failed) return 0.0;
            
            // Check for division by zero with tolerance for floating point
            if (fabs(right) < 1e-15) {
                parser_fail(parser, op.char_index, "Division by zero");
                return 0.0;
            }
            result /= right;
        }
        else {
            break;
        }
    }
    
    return result;
}

// expr := term { ('+' | '-') term }
// Top-level expression parser handling addition and subtraction
static double parse_additive_expression(ExpressionParser* parser) {
    double result = parse_multiplicative_expression(parser);
    if (parser->failed) return 0.0;
    
    while (1) {
        if (parser_accept(parser, TK_ADD)) {
            double right = parse_multiplicative_expression(parser);
            if (parser->failed) return 0.0;
            result += right;
        }
        else if (parser_accept(parser, TK_SUBTRACT)) {
            double right = parse_multiplicative_expression(parser);
            if (parser->failed) return 0.0;
            result -= right;
        }
        else {
            break;
        }
    }
    
    return result;
}

// Main evaluation function that coordinates scanning and parsing
static double evaluate_expression_string(const char* expression, int length, int* error_location) {
    Scanner scan;
    ExpressionParser parser;
    
    initialize_scanner(&scan, expression, length);
    parser.scanner = &scan;
    parser.failed = 0;
    parser.fail_position = 0;
    parser.fail_reason = NULL;
    
    double computation_result = parse_additive_expression(&parser);
    
    // Check for extra tokens after valid expression
    if (!parser.failed && !parser_accept(&parser, TK_END)) {
        parser_fail(&parser, scanner_current(&scan).char_index, "Extra characters after expression");
    }
    
    // Handle lexer-level errors
    if (scan.has_error && !parser.failed) {
        parser_fail(&parser, scan.error_position, "Invalid character in input");
    }
    
    if (parser.failed) {
        *error_location = parser.fail_position;
        return 0.0;
    }
    
    *error_location = 0;
    return computation_result;
}

// Check if line starts with '#' after optional whitespace (Python-style comments)
static int is_comment(const char* line) {
    while (*line && isspace(*line)) line++;
    return *line == '#';
}

// Portable string duplication
static char* duplicate_string(const char* original) {
    if (!original) return NULL;
    size_t len = strlen(original) + 1;
    char* copy = malloc(len);
    if (copy) memcpy(copy, original, len);
    return copy;
}

// Process single input file and create corresponding output file
static void handle_single_file(const char* input_file, const char* output_folder, 
                              const char* first_name, const char* family_name, const char* id) {
    FILE* input = fopen(input_file, "r");
    if (!input) {
        fprintf(stderr, "Failed to open: %s\n", input_file);
        return;
    }
    
    char* file_content = NULL;
    size_t total_size = 0;
    size_t allocated = 0;
    char read_buffer[1024];
    
    // Read file while skipping comment lines
    while (fgets(read_buffer, sizeof(read_buffer), input)) {
        if (is_comment(read_buffer)) continue;
        
        size_t chunk_size = strlen(read_buffer);
        if (total_size + chunk_size + 1 > allocated) {
            allocated = allocated ? allocated * 2 : 2048;
            char* new_memory = realloc(file_content, allocated);
            if (!new_memory) {
                fprintf(stderr, "Memory allocation failed\n");
                free(file_content);
                fclose(input);
                return;
            }
            file_content = new_memory;
        }
        memcpy(file_content + total_size, read_buffer, chunk_size);
        total_size += chunk_size;
    }
    fclose(input);
    
    if (!file_content) {
        file_content = duplicate_string("");
        total_size = 0;
    } else {
        file_content[total_size] = '\0';
    }
    
    int problem_position;
    double answer = evaluate_expression_string(file_content, total_size, &problem_position);
    
    // Generate output filename with student information
    char output_file_path[MAX_OUTPUT_PATH];
    const char* base_name = strrchr(input_file, '/');
    base_name = base_name ? base_name + 1 : input_file;
    
    char name_without_extension[MAX_FILENAME];
    strncpy(name_without_extension, base_name, sizeof(name_without_extension) - 1);
    name_without_extension[sizeof(name_without_extension) - 1] = '\0';
    char* dot_location = strrchr(name_without_extension, '.');
    if (dot_location && strcmp(dot_location, ".txt") == 0) *dot_location = '\0';
    
    int path_len = snprintf(output_file_path, sizeof(output_file_path), "%s/%s_%s_%s_%s.txt",
             output_folder, name_without_extension, first_name, family_name, id);
    
    if (path_len >= (int)sizeof(output_file_path)) {
        fprintf(stderr, "Output path too long: %s\n", output_file_path);
        free(file_content);
        return;
    }
    
    // Create output directory if it doesn't exist
    struct stat dir_check;
    if (stat(output_folder, &dir_check) == -1) {
        mkdir(output_folder, 0775);
    }
    
    FILE* output = fopen(output_file_path, "w");
    if (!output) {
        fprintf(stderr, "Cannot create output: %s\n", output_file_path);
        free(file_content);
        return;
    }
    
    // Write result or error with position
    if (problem_position != 0) {
        fprintf(output, "ERROR:%d\n", problem_position);
    } else {
        // Print integers without decimals, floats with full precision
        if (fabs(answer - round(answer)) < 1e-12) {
            fprintf(output, "%.0f\n", answer);
        } else {
            fprintf(output, "%.15g\n", answer);
        }
    }
    
    fclose(output);
    free(file_content);
}

// Process all .txt files in a directory
static void process_directory_files(const char* input_folder, const char* output_folder,
                                   const char* first_name, const char* family_name, const char* id) {
    DIR* folder = opendir(input_folder);
    if (!folder) {
        fprintf(stderr, "Cannot open directory: %s\n", input_folder);
        return;
    }
    
    struct dirent* entry;
    while ((entry = readdir(folder))) {
        const char* filename = entry->d_name;
        size_t name_length = strlen(filename);
        
        // Process only .txt files (case sensitive)
        if (name_length > 4 && strcmp(filename + name_length - 4, ".txt") == 0) {
            char full_path[MAX_FILENAME];
            snprintf(full_path, sizeof(full_path), "%s/%s", input_folder, filename);
            
            struct stat file_info;
            if (stat(full_path, &file_info) == 0 && S_ISREG(file_info.st_mode)) {
                printf("Processing: %s\n", filename);
                handle_single_file(full_path, output_folder, first_name, family_name, id);
            }
        }
    }
    closedir(folder);
}

// Generate default output directory name per assignment requirements
static void generate_output_folder_name(const char* input_file, const char* user, const char* id, char* buffer, size_t buffer_size) {
    const char* base = strrchr(input_file, '/');
    base = base ? base + 1 : input_file;
    
    char no_extension[MAX_FILENAME];
    strncpy(no_extension, base, sizeof(no_extension) - 1);
    no_extension[sizeof(no_extension) - 1] = '\0';
    
    char* dot = strrchr(no_extension, '.');
    if (dot) *dot = '\0';
    
    snprintf(buffer, buffer_size, "%s_%s_%s", no_extension, user, id);
}

// Parse command line arguments with support for options
static int parse_command_line(int arg_count, char* arg_values[], ProgramOptions* options) {
    options->input_path[0] = '\0';
    options->output_path[0] = '\0';
    options->directory_mode = 0;
    
    for (int i = 1; i < arg_count; i++) {
        if (strcmp(arg_values[i], "-d") == 0 || strcmp(arg_values[i], "--dir") == 0) {
            if (i + 1 >= arg_count) {
                fprintf(stderr, "Option %s requires directory path\n", arg_values[i]);
                return 0;
            }
            strncpy(options->input_path, arg_values[++i], MAX_FILENAME - 1);
            options->input_path[MAX_FILENAME - 1] = '\0';
            options->directory_mode = 1;
        }
        else if (strcmp(arg_values[i], "-o") == 0 || strcmp(arg_values[i], "--output-dir") == 0) {
            if (i + 1 >= arg_count) {
                fprintf(stderr, "Option %s requires directory path\n", arg_values[i]);
                return 0;
            }
            strncpy(options->output_path, arg_values[++i], MAX_FILENAME - 1);
            options->output_path[MAX_FILENAME - 1] = '\0';
        }
        else if (arg_values[i][0] == '-') {
            fprintf(stderr, "Unknown option: %s\n", arg_values[i]);
            return 0;
        }
        else {
            strncpy(options->input_path, arg_values[i], MAX_FILENAME - 1);
            options->input_path[MAX_FILENAME - 1] = '\0';
        }
    }
    
    if (options->input_path[0] == '\0') {
        fprintf(stderr, "No input specified\n");
        fprintf(stderr, "Usage: %s [-d DIR | --dir DIR] [-o OUTDIR | --output-dir OUTDIR] input.txt\n", arg_values[0]);
        return 0;
    }
    
    return 1;
}

int main(int arg_count, char* arg_values[]) {
    ProgramOptions config;
    
    if (!parse_command_line(arg_count, arg_values, &config)) {
        return 1;
    }
    
    // Student information for output file naming
    const char* first_name = "Nandana";
    const char* family_name = "Subhash";
    const char* student_id = "241ADB029";
    const char* username = "nandana";
    
    const char* output_directory;
    char auto_directory[MAX_FOLDER_NAME];
    
    // Determine output directory: custom or auto-generated
    if (config.output_path[0] != '\0') {
        output_directory = config.output_path;
    } else {
        if (config.directory_mode) {
            snprintf(auto_directory, sizeof(auto_directory), "%s_%s_%s", 
                    config.input_path, username, student_id);
        } else {
            generate_output_folder_name(config.input_path, username, student_id, auto_directory, sizeof(auto_directory));
        }
        output_directory = auto_directory;
    }
    
    printf("Output will be saved to: %s\n", output_directory);
    
    // Create output directory if it doesn't exist
    struct stat dir_info;
    if (stat(output_directory, &dir_info) == -1) {
        mkdir(output_directory, 0775);
    }
    
    // Process single file or directory based on mode
    if (config.directory_mode) {
        process_directory_files(config.input_path, output_directory, first_name, family_name, student_id);
    } else {
        handle_single_file(config.input_path, output_directory, first_name, family_name, student_id);
    }
    
    printf("Completed processing. Output location: %s\n", output_directory);
    return 0;
}