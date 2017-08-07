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

use $crate::errors::{Error, Result, ErrorKind};
use $crate::span::{Span, Spanned};
use $crate::eval::store::Store;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    $($variant($variant_type),)*
    Empty
}

#[allow(dead_code)]
pub fn eval<'a>(program: $program_type<'a>) -> Result<'a, Value> {
    Evaluator::new().eval(program)
}

pub struct Evaluator {
    #[allow(dead_code)]
    store: Store<$ident_type, Value>,
}

fn eval_error<'a, T: AsRef<str>>(msg: T) -> ErrorKind<'a> {
    ErrorKind::Eval(msg.as_ref().to_string())
}
type OpResult = ::std::result::Result<Value, String>;

impl Evaluator {
    pub fn new() -> Evaluator {
        Evaluator {
            store: Store::new(),
        }
    }

    pub fn eval<'a>(&mut self, program: $program_type<'a>) -> Result<'a, Value> {
        self.eval_block_nopush(program)
    }

    pub fn eval_block<'a>(
        &mut self,
        sp: Span<'a>,
        block: $block_type<'a>
    ) -> Result<'a, Value> {
        self.store.push();
        let ret = self.eval_block_nopush(block);
        match self.store.pop() {
            Some(parent_store) => { self.store = *parent_store; },
            None => {
                return Err(Error::spanned(eval_error("invalid descoping"), sp));
            }
        }
        ret
    }

    pub fn eval_block_nopush<'a>(&mut self, mut block: $block_type<'a>) -> Result<'a, Value> {
        block.reverse();
        self.eval_reversed_block(block)
    }

    pub fn eval_reversed_block<'a>(&mut self, mut block: $block_type<'a>) -> Result<'a, Value> {
        if let Some(first) = block.pop() {
            let value = self.eval_statement(first)?;
            if block.is_empty() {
                Ok(value)
            } else {
                self.eval_reversed_block(block)
            }
        } else {
            Ok(Value::Empty)
        }
    }

    pub fn eval_statement<'a>(
        &mut self,
        statement: Spanned<'a, Statement<'a>>
    ) -> Result<'a, Value> {
        match statement.item {
            $(
                $stmt_type::$stmt_variant($($args)*) => {
                    stmt_eval_arm!(self, $stmt_eval_func($($params),*))
                },
            )*
        }
    }

    pub fn eval_expression<'a>(&mut self, expr: Spanned<'a, Expression<'a>>) -> Result<'a, Value> {
        let Spanned { item, span: sp } = expr;
        match item {
            Expression::Literal(literal)           => self.eval_literal(sp, literal),
            Expression::Infix { op, left, right }  => self.eval_infix(sp, op, *left, *right),
            Expression::Prefix { op, right }       => self.eval_prefix(sp, op, *right),
            Expression::Postfix { op, left }       => self.eval_postfix(sp, op, *left),
            Expression::Identifier(ident)          => self.eval_identifier(sp, ident),
            Expression::Block(block)               => self.eval_block(sp, block),
        }
    }

    pub fn eval_literal<'a>(&mut self, sp: Span<'a>, literal: Literal) -> Result<'a, Value> {
        #[allow(unreachable_patterns)]
        match literal {
            $(
                $value_source(value) => Ok(Value::$variant(value)),
            )*
            _ => Err(Error::spanned(eval_error(format!("unknown literal: '{}'", literal)), sp))
        }
    }

    pub fn eval_infix<'a>(
        &mut self,
        sp: Span<'a>,
        op: InfixOp,
        left: Spanned<'a, Expression<'a>>,
        right: Spanned<'a, Expression<'a>>
    ) -> Result<'a, Value> {
        #[allow(unused_variables)]
        let left_value = self.eval_expression(left)?;
        #[allow(unused_variables)]
        let right_value = self.eval_expression(right)?;

        #[allow(unreachable_patterns)]
        match op {
            $(
                $infix_type => $infix_op_fn(left_value, right_value).map_err(
                    |e| Error::spanned(eval_error(e), sp)),
            )*
            _ => Err(Error::spanned(eval_error(
                format!("unhandled infix operator: '{}'", op)), sp))
        }
    }

    #[allow(unused_variables)]
    pub fn eval_prefix<'a>(
        &mut self,
        sp: Span<'a>,
        op: PrefixOp,
        right: Spanned<'a, Expression<'a>>
    ) -> Result<'a, Value> {
        #[allow(unused_variables)]
        let right_value = self.eval_expression(right)?;

        #[allow(unreachable_patterns)]
        match op {
            $(
                $prefix_type => $prefix_op_fn(right_value).map_err(
                    |e| Error::spanned(eval_error(e), sp)),
            )*
            _ => Err(Error::spanned(eval_error(
                format!("unhandled prefix operator: '{}'", op)), sp))
        }
    }

    #[allow(unused_variables)]
    pub fn eval_postfix<'a>(
        &mut self,
        sp: Span<'a>,
        op: PostfixOp,
        left: Spanned<'a, Expression<'a>>
    ) -> Result<'a, Value> {
        #[allow(unused_variables)]
        let left_value = self.eval_expression(left)?;

        #[allow(unreachable_patterns)]
        match op {
            $(
                $postfix_type => $postfix_op_fn(left_value).map_err(
                    |e| Error::spanned(eval_error(e), sp)),
            )*
            _ => Err(Error::spanned(eval_error(
                format!("unhandled postfix operator: '{}'", op)), sp))
        }
    }

    pub fn eval_identifier<'a>(
        &mut self,
        sp: Span<'a>,
        ident: $ident_type
    ) -> Result<'a, Value> {
        match self.store.get(&ident) {
            Some(value) => Ok(value),
            None => Err(Error::spanned(eval_error(format!("identifier not found: '{}'", ident)),
                sp))
        }
    }

    #[allow(dead_code)]
    pub fn eval_assignment<'a>(
        &mut self,
        ident: Spanned<'a, $ident_type>,
        expr: Spanned<'a, Expression<'a>>
    ) -> Result<'a, Value> {
        let rvalue = self.eval_expression(expr)?;
        self.store.set(ident.item, rvalue);
        Ok(Value::Empty)
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

        group_tokens![Token: Token::LParen, Token::RParen];
        block_tokens![Token: None];

        parser![
            token_type: Token,
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

        fn add_values(left: Value, right: Value) -> OpResult {
            match (left, right) {
                (Value::Integer(l), Value::Integer(r)) => Ok(Value::Integer(l + r)),
                (_, _) => Err("addition only valid between two integers".to_string()),
            }
        }
        fn multiply_values(left: Value, right: Value) -> OpResult {
            match (left, right) {
                (Value::Integer(l), Value::Integer(r)) => Ok(Value::Integer(l * r)),
                (_, _) => Err("multiplication only valid between two integers".to_string()),
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
