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

//  DATA STRUCTURES 
typedef enum {
    TOK_NUMBER,    // Numeric literal (integer or float)
    TOK_PLUS,      // Addition operator '+'
    TOK_MINUS,     // Subtraction operator '-'
    TOK_STAR,      // Multiplication operator '*'
    TOK_SLASH,     // Division operator '/'
    TOK_POW,       // Exponentiation operator ''  
    TOK_LPAREN,    // Left parenthesis '('          
    TOK_RPAREN,    // Right parenthesis ')'
    TOK_EOF,       // End of input
    TOK_ERROR      // Invalid token
} TokenType;

typedef struct {
    TokenType type;
    double value;
    int start_pos;
} Token;

typedef struct {
    const char *input;
    int length;
    int position;
    Token current_token;
    int has_error;
    int error_position;
} Parser;

// Global variables
char *input_dir = NULL;
char *output_dir = NULL;
char *single_file = NULL;

// FUNCTION DECLARATIONS 
// ADDED THESE DECLARATIONS TO FIX IMPLICIT DECLARATION ERRORS
double parse_expression(Parser *parser);
double parse_term(Parser *parser);
double parse_power(Parser *parser);
double parse_primary(Parser *parser);
void next_token(Parser *parser);

//  UTILITY FUNCTIONS 
int is_whitespace(char c) {
    return c == ' ' || c == '\t' || c == '\r' || c == '\n';
}

void fail(Parser *parser, int pos) {
    if (!parser->has_error) {
        parser->has_error = 1;
        parser->error_position = pos;
    }
}

void create_directory_if_missing(const char *path) {
    struct stat st;
    if (stat(path, &st) != 0) {
        mkdir(path, 0775);
    }
}

//  TOKENIZER 
int should_skip_line(const char *input, int *pos, int length) {
    // REMOVED unused variable 'original_pos'
    int temp_pos = *pos;
    
    // Skip whitespace
    while (temp_pos < length && is_whitespace(input[temp_pos])) {
        temp_pos++;
    }
    
    // Skip comment lines
    if (temp_pos < length && input[temp_pos] == '#') {
        while (temp_pos < length && input[temp_pos] != '\n') temp_pos++;
        if (temp_pos < length && input[temp_pos] == '\n') temp_pos++;
        *pos = temp_pos;
        return 1;
    }
    
    return 0;
}

void next_token(Parser *parser) {
    // Skip whitespace and comments
    while (parser->position < parser->length) {
        if (should_skip_line(parser->input, &parser->position, parser->length)) continue;
        if (is_whitespace(parser->input[parser->position])) {
            parser->position++;
        } else break;
    }
    
    // End of input
    if (parser->position >= parser->length) {
        parser->current_token.type = TOK_EOF;
        parser->current_token.start_pos = parser->position + 1;
        return;
    }
    
    char c = parser->input[parser->position];
    int start_pos = parser->position + 1;
    
    // Parse numbers
    if (isdigit(c) || c == '.' || 
        ((c == '+' || c == '-') && parser->position + 1 < parser->length && 
         (isdigit(parser->input[parser->position + 1]) || parser->input[parser->position + 1] == '.'))) {
        
        char *end_ptr;
        double value = strtod(parser->input + parser->position, &end_ptr);
        
        if (end_ptr > parser->input + parser->position) {
            parser->current_token.type = TOK_NUMBER;
            parser->current_token.value = value;
            parser->current_token.start_pos = start_pos;
            parser->position = end_ptr - parser->input;
            return;
        }
    }
    
    // Parse operators
    parser->current_token.start_pos = start_pos;
    switch (c) {
        case '+': parser->current_token.type = TOK_PLUS; parser->position++; break;
        case '-': parser->current_token.type = TOK_MINUS; parser->position++; break;
        case '*': 
            if (parser->position + 1 < parser->length && parser->input[parser->position + 1] == '*') {
                parser->current_token.type = TOK_POW; parser->position += 2;  // FIXED: POW not POM
            } else {
                parser->current_token.type = TOK_STAR; parser->position++;
            }
            break;
        case '/': parser->current_token.type = TOK_SLASH; parser->position++; break;
        case '(': parser->current_token.type = TOK_LPAREN; parser->position++; break;  // FIXED: LPAREN not IPAREN
        case ')': parser->current_token.type = TOK_RPAREN; parser->position++; break;
        default: parser->current_token.type = TOK_ERROR; fail(parser, start_pos); break;
    }
}

void init_parser(Parser *parser, const char *input, int length) {
    parser->input = input;
    parser->length = length;
    parser->position = 0;
    parser->has_error = 0;
    parser->error_position = -1;
    next_token(parser);
}

//  PARSER (Recursive Descent) 

// Parse numbers and parentheses
double parse_primary(Parser *parser) {
    if (parser->has_error) return 0;
    
    Token token = parser->current_token;
    
    if (token.type == TOK_NUMBER) {
        next_token(parser);
        return token.value;
    }
    else if (token.type == TOK_LPAREN) {  // FIXED: LPAREN not IPAREN
        next_token(parser);
        double result = parse_expression(parser);
        if (parser->has_error) return 0;
        
        if (parser->current_token.type != TOK_RPAREN) {
            fail(parser, parser->current_token.start_pos);
            return 0;
        }
        next_token(parser);
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
    
    while (!parser->has_error && parser->current_token.type == TOK_POW) {  // FIXED: POW not POM
        int op_pos = parser->current_token.start_pos;
        next_token(parser);
        double right = parse_power(parser); // Right-associative
        
        if (parser->has_error) return 0;
        
        // Error checking
        if (left == 0 && right < 0) {
            fail(parser, op_pos);
            return 0;
        }
        left = pow(left, right);
    }
    return left;
}

// Parse multiplication and division
double parse_term(Parser *parser) {
    if (parser->has_error) return 0;
    
    double result = parse_power(parser);
    
    while (!parser->has_error && (parser->current_token.type == TOK_STAR || 
                                 parser->current_token.type == TOK_SLASH)) {
        Token op = parser->current_token;
        next_token(parser);
        double right = parse_power(parser);
        
        if (parser->has_error) return 0;
        
        if (op.type == TOK_STAR) {
            result *= right;
        } else {
            if (right == 0.0) {
                fail(parser, op.start_pos);
                return 0;
            }
            result /= right;
        }
    }
    return result;
}

// Parse addition and subtraction
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
        } else {
            result -= right;
        }
    }
    return result;
}

// Main evaluation
double evaluate_expression(Parser *parser) {
    double result = parse_expression(parser);
    
    // Check for extra tokens
    if (!parser->has_error && parser->current_token.type != TOK_EOF) {
        fail(parser, parser->current_token.start_pos);
        return 0;
    }
    return result;
}

//  FILE OPERATIONS 

char* build_output_path(const char *input_path) {
    static char output_path[1024];
    const char *filename = strrchr(input_path, '/');
    filename = filename ? filename + 1 : input_path;
    
    char base_name[256];
    strncpy(base_name, filename, sizeof(base_name) - 1);
    base_name[sizeof(base_name) - 1] = '\0';
    
    // Remove .txt extension
    char *dot = strrchr(base_name, '.');
    if (dot) *dot = '\0';
    
    if (output_dir) {
        snprintf(output_path, sizeof(output_path), "%s/%s_nandana_subhash_241ADB029.txt", 
                 output_dir, base_name);
    } else {
        char default_dir[512];
        snprintf(default_dir, sizeof(default_dir), "%s_nandana_subhash_241ADB029", base_name);
        create_directory_if_missing(default_dir);
        snprintf(output_path, sizeof(output_path), "%s/%s_nandana_subhash_241ADB029.txt", 
                 default_dir, base_name);
    }
    return output_path;
}

void process_single_file(const char *input_path) {
    FILE *file = fopen(input_path, "r");
    if (!file) {
        perror("Error opening file");
        exit(1);
    }
    
    // Read file
    fseek(file, 0, SEEK_END);
    long size = ftell(file);
    fseek(file, 0, SEEK_SET);
    
    char *input = malloc(size + 1);
    if (!input) {
        perror("Memory allocation failed");
        fclose(file);
        exit(1);
    }
    
    //  Check fread return value
    size_t bytes_read = fread(input, 1, size, file);
    if (bytes_read != (size_t)size) {
        perror("Error reading file");
        free(input);
        fclose(file);
        exit(1);
    }
    input[bytes_read] = '\0';
    fclose(file);
    
    // Parse and evaluate
    Parser parser;
    init_parser(&parser, input, bytes_read);
    double result = evaluate_expression(&parser);
    
    // Write result
    char *output_path = build_output_path(input_path);
    FILE *output = fopen(output_path, "w");
    if (!output) {
        perror("Error opening output file");
        free(input);
        exit(1);
    }
    
    if (parser.has_error) {
        fprintf(output, "ERROR:%d\n", parser.error_position);
    } else {
        // Print integers without decimals
        if (fabs(result - round(result)) < 1e-12) {
            fprintf(output, "%.0f\n", result);
        } else {
            fprintf(output, "%.15g\n", result);
        }
    }
    fclose(output);
    free(input);
}

void process_directory(const char *dir_path) {
    DIR *dir = opendir(dir_path);
    if (!dir) {
        perror("Error opening directory");
        exit(1);
    }
    
    if (output_dir) create_directory_if_missing(output_dir);
    
    struct dirent *entry;
    while ((entry = readdir(dir)) != NULL) {
        const char *name = entry->d_name;
        if (strlen(name) > 4 && strcmp(name + strlen(name) - 4, ".txt") == 0) {
            char path[1024];
            snprintf(path, sizeof(path), "%s/%s", dir_path, name);
            printf("Processing: %s\n", name);
            process_single_file(path);
        }
    }
    closedir(dir);
}

void parse_args(int argc, char *argv[]) {
    for (int i = 1; i < argc; i++) {
        if ((strcmp(argv[i], "-d") == 0 || strcmp(argv[i], "--dir") == 0) && i + 1 < argc) {
            input_dir = argv[++i];
        }
        else if ((strcmp(argv[i], "-o") == 0 || strcmp(argv[i], "--output-dir") == 0) && i + 1 < argc) {
            output_dir = argv[++i];
        }
        else if (argv[i][0] == '-') {
            fprintf(stderr, "Unknown option: %s\n", argv[i]);
            exit(1);
        }
        else {
            single_file = argv[i];
        }
    }
    
    if (input_dir && single_file) {
        fprintf(stderr, "Cannot use both directory and single file\n");
        exit(1);
    }
}

//  MAIN FUNCTION 
int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s [-d DIR] [-o OUTDIR] [input.txt]\n", argv[0]);
        return 1;
    }
    
    parse_args(argc, argv);
    
    if (output_dir) create_directory_if_missing(output_dir);
    
    if (input_dir) {
        process_directory(input_dir);
    } else {
        process_single_file(single_file);
    }
    
    return 0;
}