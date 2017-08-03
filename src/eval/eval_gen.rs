#[macro_export]
macro_rules! stmt_eval_arm {
    ($self:ident, eval_expression($($eval_params:tt),*)) => (
        $self.eval_expression($($eval_params),*)
    );
    ($self:ident, eval_assignment($($eval_params:tt),*)) => (
        $self.eval_assignment($($eval_params),*)
    );
    ($self:ident, $stmt_eval_func:ident($($eval_params:tt),*)) => (
        $stmt_eval_func($($eval_params),*, $self)
    );
}


#[macro_export]
macro_rules! evaluator {
    (
        program_type: $program_type:tt,
        block_type: $block_type:tt,
        identifier_type: $ident_type:ty,
        expression_type: $expr_type:ty,
        values: [
            $($value_source:path => $variant:ident<$variant_type:ty>),*
        ],
        eval_statement: [
            $($stmt_type:tt::$stmt_variant:ident($($args:tt)*)
                => $stmt_eval_func:ident($($params:tt),*)),*
        ],
        infix: ($infix_op_type:ty, [
            $($infix_type:path => $infix_op_fn:ident),*
        ]),
        prefix: ($prefix_op_type:ty, [
            $($prefix_type:path => $prefix_op_fn:ident),*
        ]),
        postfix: ($postfix_op_type:ty, [
            $($postfix_type:path => $postfix_op_fn:ident),*
        ])
    ) => (

use $crate::errors;
use $crate::span::Spanned;
use $crate::eval::store::Store;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    $($variant($variant_type),)*
    Empty
}

#[allow(dead_code)]
pub fn eval<'a>(program: $program_type<'a>) -> errors::Result<'a, Value> {
    Evaluator::new().eval(program)
}

pub struct Evaluator {
    #[allow(dead_code)]
    store: Store<$ident_type, Value>,
}

impl Evaluator {
    pub fn new() -> Evaluator {
        Evaluator {
            store: Store::new(),
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
                $stmt_type::$stmt_variant($($args)*) => {
                    stmt_eval_arm!(self, $stmt_eval_func($($params),*))
                },
            )*
        }
    }

    pub fn eval_expression(&mut self, expression: Spanned<Expression>) -> Value {
        match expression.item {
            Expression::Literal(literal)           => self.eval_literal(literal),
            Expression::Infix { op, left, right }  => self.eval_infix(op, *left, *right),
            Expression::Prefix { op, right }       => self.eval_prefix(op, *right),
            Expression::Postfix { op, left }       => self.eval_postfix(op, *left),
            Expression::Identifier(ident)          => self.eval_identifier(ident),
        }
    }

    pub fn eval_literal(&mut self, literal: Literal) -> Value {
        match literal {
            $(
                $value_source(value) => Value::$variant(value)
            ),*
        }
    }

    pub fn eval_infix(
        &mut self,
        op: InfixOp,
        left: Spanned<Expression>,
        right: Spanned<Expression>
    ) -> Value {
        #[allow(unused_variables)]
        let left_value = self.eval_expression(left);
        #[allow(unused_variables)]
        let right_value = self.eval_expression(right);

        match op {
            $(
                $infix_type => $infix_op_fn(left_value, right_value)
            ),*
        }
    }

    pub fn eval_prefix(
        &mut self,
        op: PrefixOp,
        right: Spanned<Expression>
    ) -> Value {
        #[allow(unused_variables)]
        let right_value = self.eval_expression(right);

        match op {
            $(
                $prefix_type => $prefix_op_fn(right_value)
            ),*
        }
    }

    pub fn eval_postfix(
        &mut self,
        op: PostfixOp,
        left: Spanned<Expression>
    ) -> Value {
        #[allow(unused_variables)]
        let left_value = self.eval_expression(left);

        match op {
            $(
                $postfix_type => $postfix_op_fn(left_value)
            ),*
        }
    }

    pub fn eval_identifier(
        &mut self,
        ident: $ident_type
    ) -> Value {
        match self.store.get(ident) {
            Some(&ref value) => value.clone(),
            None => Value::Empty
        }
    }

    pub fn eval_assignment(
        &mut self,
        ident: $ident_type,
        expr: Spanned<Expression>
    ) -> Value {
        let rvalue = self.eval_expression(expr);
        self.store.set(ident, rvalue);
        Value::Empty
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
        use super::parser::{Program, Block, Statement, Expression, Literal, InfixOp, PrefixOp,
            PostfixOp, Identifier};
        // use super::eval_expression;

        evaluator![
            program_type: Program,
            block_type: Block,
            identifier_type: Identifier,
            expression_type: Expression,
            values: [
                Literal::Integer => Integer<i64>
            ],
            eval_statement: [
                Statement::ExpressionStmt(expr) => eval_expression(expr)
            ],
            infix: (InfixOp, [
                InfixOp::Multiply => multiply_values,
                InfixOp::Add      => add_values
            ]),
            prefix: (PrefixOp, []),
            postfix: (PostfixOp, [])
        ];

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
}
