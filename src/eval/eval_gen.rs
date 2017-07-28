#[macro_export]
macro_rules! evaluator {
    (
        program_type: $program_type:tt,
        block_type: $block_type:tt,
        identifier_type: $ident_type:ty,
        values: [
            $($variant:ident($($variant_type:ty),*)),*
        ],
        eval_statement: [
            $($stmt_type:tt::$stmt_variant:ident(($($args:tt)*))
                => $stmt_eval_func:ident(($($params:tt),*))),*
        ]
    ) => (

use $crate::errors;
use $crate::span::Spanned;
use $crate::eval::context::Context;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    $($variant($($variant_type),*),)*
    Empty
}

#[allow(dead_code)]
pub fn eval<'a>(program: $program_type<'a>) -> errors::Result<'a, Value> {
    Evaluator::new().eval(program)
}

pub struct Evaluator {
    #[allow(dead_code)]
    context: Context<$ident_type, Value>,
}

impl Evaluator {
    pub fn new() -> Evaluator {
        Evaluator {
            context: Context::new(),
        }
    }

    pub fn eval<'a>(&mut self, mut program: $program_type<'a>) -> errors::Result<'a, Value> {
        Ok(self.eval_block(&mut program))
    }

    pub fn eval_block<'a>(&mut self, block: &mut $block_type<'a>) -> Value {
        block.reverse();
        self.eval_reversed_block(block)
    }

    pub fn eval_reversed_block<'a>(&mut self, block: &mut $block_type<'a>) -> Value {
        if let Some(first) = block.pop() {
            let value = self.eval_statement(first);
            if block.is_empty() {
                value
            } else {
                self.eval_reversed_block(block)
            }
        } else {
            Value::Empty
        }
    }

    pub fn eval_statement<'a>(&mut self, statement: Spanned<'a, Statement>) -> Value {
        match statement.item {
            $(
                $stmt_type::$stmt_variant($($args)*) => $stmt_eval_func($($params),*, self),
            )*
        }
    }
}

    ); // end implementation macro expression arm
}

#[cfg(test)]
#[allow(dead_code)]
mod simple_calc {

    mod lexer {
        use lex::rules::{
            PTN_INT, convert_int,
            PTN_IDENTIFIER, convert_identifier,
        };

        lexer![
            r"\("                                   => LParen,
            r"\)"                                   => RParen,
            r"\+"                                   => Plus,
            r"\*"                                   => Asterisk,
            PTN_INT         => convert_int          => IntLiteral<i64>,
            PTN_IDENTIFIER  => convert_identifier   => Identifier<String>,
        ];
    }

    mod parser {
        use super::lexer::Token;
        use parse::precedence::StandardPrecedence;

        parser![
            token_type: Token,
            group_tokens: (Token::LParen, Token::RParen),
            statements: [
                ExpressionStmt(expression<value>) := {expression<value>},
            ],
            literals: [
                Token::IntLiteral => Integer<i64>,
            ],
            identifier_token: Token::Identifier,
            precedence_type: StandardPrecedence,
            prefix: (StandardPrecedence::Prefix, []),
            infix: [
                Token::Asterisk => Multiply   => StandardPrecedence::Product,
                Token::Plus     => Add        => StandardPrecedence::Sum,
            ]
        ];
    }


    mod evaluator {
        use super::parser::{Program, Block, Statement};
        use super::eval_expression;

        evaluator![
            program_type: Program,
            block_type: Block,
            identifier_type: String,
            values: [
                Integer(i64)
            ],
            eval_statement: [
                Statement::ExpressionStmt((expr)) => eval_expression((expr))
            ]
        ];
    }

    #[test]
    fn test_simple_calc() {
        let tokens: Vec<Spanned<lexer::Token>> = lexer::lex("5 * (9 + 2)").unwrap();
        println!("{:?}", tokens);
        let ast = parser::parse(&tokens).unwrap();
        println!("{:?}", ast);
        let value = evaluator::eval(ast).unwrap();
        println!("{:?}", value);
    }

    use span::Spanned;
    use self::parser::{Expression, Literal, InfixOp};
    use self::evaluator::{Evaluator, Value};

    fn eval_expression(expr: (Spanned<Expression>), evaluator: &mut Evaluator) -> Value {
        match expr.item {
            Expression::Literal(literal)           => eval_literal(literal, evaluator),
            Expression::Infix { op, left, right }  => eval_infix(op, *left, *right, evaluator),
            Expression::Prefix { op: _, right: _ } => panic!("invalid prefix op"),
            Expression::Identifier(_)              => panic!("invalid identifier"),
        }
    }
    fn eval_literal(literal: Literal, _: &mut Evaluator) -> Value {
        match literal {
            Literal::Integer(i)    => Value::Integer(i),
        }
    }
    fn eval_infix(
        op: InfixOp,
        left: Spanned<Expression>,
        right: Spanned<Expression>,
        evaluator: &mut Evaluator
    ) -> Value {
        let left_value = eval_expression(left, evaluator);
        let right_value = eval_expression(right, evaluator);

        match op {
            InfixOp::Add      => add_values(left_value, right_value),
            InfixOp::Multiply => multiply_values(left_value, right_value),
        }
    }
    fn add_values(left: Value, right: Value) -> Value {
        match (left, right) {
            (Value::Integer(l), Value::Integer(r)) => Value::Integer(l + r),
            (_, _) => panic!("invalid add"),
        }
    }
   fn multiply_values(left: Value, right: Value) -> Value {
        match (left, right) {
            (Value::Integer(l), Value::Integer(r)) => Value::Integer(l * r),
            (_, _) => panic!("invalid multiply"),
        }
    }
}
