#![feature(plugin)]
#![plugin(sindra_plugin)]

#[macro_use] extern crate sindra;
extern crate regex;
extern crate rustyline;
extern crate clap;

mod lexer {
    use sindra::lex::rules::{
        // PTN_NUM and convert_num will treat numeric character sequences (either integer or float)
        // as floating put numbers
        PTN_NUM, convert_num,
        // PTN_IDENTIFIER and convert_identifier handle common identifier (e.g. variable names)
        // matching
        PTN_IDENTIFIER, convert_identifier,
    };

    lexer![
        // '(' and ')' provide grouped expressions
        r"\("                                   => LParen,
        r"\)"                                   => RParen,
        // mathematical operators: add, subtract, divide, multiply, raise to a power
        r"\+"                                   => Plus,
        r"-"                                    => Minus,
        r"/"                                    => Slash,
        r"\*"                                   => Asterisk,
        r"\^"                                   => Caret,
        // equal token recognized for assignment purposes
        r"="                                    => Equal,
        // keyword 'let' for variable declaration
        r"let"                                  => Let,
        // recognize numeric values
        PTN_NUM         => convert_num          => NumLiteral<f64>,
        // recognize variable names (for assignment)
        PTN_IDENTIFIER  => convert_identifier   => Identifier<String>,
    ];
}

mod parser {
    use super::lexer::Token;
    // we'll use the standard precedence rules (see sindra/src/parse/precedence.rs)
    use sindra::parse::precedence::StandardPrecedence;

    // specify the tokens used for grouped expressions (parentheses)
    // first 'Token' refers to the token enum used, Token::LParen and Token::RParen to the specific
    // variants for opening and closing a group, respectively
    group_tokens![Token: Token::LParen, Token::RParen];
    // no block statement handling in this example
    block_tokens![Token: None];
    // specify the token enum variant used as Identifiers
    identifier_token![Token: Token::Identifier, String];

    parser![
        // specify the token enum
        token_type: Token,
        // define the literals used, and their resulting values
        literals: [
            // we only have one literal type: floating point
            Token::NumLiteral => Float<f64>,
        ],
        // define the statement types
        statements: [
            // allow declarations of variables, examples: 'let x = 5' or 'let x = 5^2 + 4'
            DeclarationStmt(identifier<name>, expression<value>) := {
                token<Token::Let> identifier<name> token<Token::Equal> expression<value>
            },
            // allow updating variables
            AssignmentStmt(identifier<name>, expression<value>) := {
                identifier<name> token<Token::Equal> expression<value>
            },
            // this is the normal statement type: an single expression
            ExpressionStmt(expression<value>) := {expression<value>},
        ],
        // specify which precedence we want to use (in this case, the standard precedence)
        precedence_type: StandardPrecedence,

        // specify the prefix operators we want: unary minus '-x' and unary plus '+x',
        // along with the precedence we used for prefix operators
        prefix: (StandardPrecedence::Prefix, [
            Token::Plus     => UnaryPlus,
            Token::Minus    => UnaryMinus,
        ]),
        // specify the infix operators we want: '+', '-', '*', '/', '^', and their precedences
        infix: [
            Token::Plus     => Add        => StandardPrecedence::Sum,
            Token::Minus    => Subtract   => StandardPrecedence::Sum,
            Token::Asterisk => Multiply   => StandardPrecedence::Product,
            Token::Slash    => Divide     => StandardPrecedence::Product,
            Token::Caret    => Power      => StandardPrecedence::Power,
        ]
    ];
}

mod evaluator {

    evaluator![
        // specify the AST module (where parser! macro is run)
        ast_module: super::parser,
        // convert the literal type to a value
        values: [
            Literal::Float => Float<f64>
        ],
        // specify how to evaluate the statements: eval_assignment is a predefined method that
        // handles variable storage, and eval_expression handles expression evaluation
        eval_statement: [
            Statement::DeclarationStmt((ident, expr)) =>
                eval_declaration(ident, expr, Redeclare::Allowed, AssignReturn::Assigned),
            Statement::AssignmentStmt((ident, expr)) =>
                eval_assignment(ident, expr, AssignReturn::Assigned),
            Statement::ExpressionStmt(expr) => eval_expression(expr)
        ],
        // specify the actual methods (defined below) to handle the various infix operators
        infix: [
            InfixOp::Multiply => multiply_values,
            InfixOp::Add      => add_values,
            InfixOp::Subtract => subtract_values,
            InfixOp::Divide   => divide_values,
            InfixOp::Power    => raise
        ],
        // specify the actual methods (defined below) to handle the various prefix operators
        prefix: [
            PrefixOp::UnaryMinus   => negate_value,
            // note that posate_value doesn't actually do anything
            PrefixOp::UnaryPlus    => posate_value
        ],
    ];

    // define the methods used for the various operators, as specified in the evaluator! macro.
    // OpResult is a Result<Value, String>
    // Since we only have one Value type (Float), we only really have to worry about the case
    // where we are applying these methods upon one or two Floats; however, since the Value enum
    // always has an 'Empty' variant, we need to handle the catch-all '_' case, including the
    // ever-important 'butt case' (_,_) and throw errors appropriately

    fn add_values(left: Value, right: Value) -> OpResult {
        match (left, right) {
            (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l + r)),
            (_, _) => Err("addition only valid between two floats".to_string()),
        }
    }
    fn subtract_values(left: Value, right: Value) -> OpResult {
        match (left, right) {
            (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l - r)),
            (_, _) => Err("subtraction only valid between two floats".to_string()),
        }
    }
    fn multiply_values(left: Value, right: Value) -> OpResult {
        match (left, right) {
            (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l * r)),
            (_, _) => Err("multiplication only valid between two floats".to_string()),
        }
    }
    fn divide_values(left: Value, right: Value) -> OpResult {
        match (left, right) {
            (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l / r)),
            (_, _) => Err("division only valid between two floats".to_string()),
        }
    }
    fn raise(left: Value, right: Value) -> OpResult {
        match (left, right) {
            (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l.powf(r))),
            (_, _) => Err("raising only valid between two floats".to_string()),
        }
    }
    fn negate_value(right: Value) -> OpResult {
        match right {
            Value::Float(r) => Ok(Value::Float(-r)),
            _ => Err("unary negation only valid for floats".to_string())
        }
    }
    fn posate_value(right: Value) -> OpResult {
        match right {
            Value::Float(r) => Ok(Value::Float(r)),
            _ => Err("unary posation only valid for floats".to_string())
        }
    }

}

// define an interpreter and repl using the generate lex and parse methods, and the Evaluator object
interp_repl!(lexer: lexer::lex, parser: parser::parse, evaluator: evaluator::Evaluator);

// main is simple: just call the 'start' method that the interp_repl! macro produces
fn main() {
    start("SimpleCalc", "0.0.1");
}
