#[macro_export]
macro_rules! stmt_eval_arm {
    ($self:ident, eval_expression($($eval_params:tt),*)) => (
        $self.eval_expression($($eval_params),*)
    );
    ($self:ident, eval_assignment($($eval_params:tt),*)) => (
        $self.eval_assignment($($eval_params),*)
    );
    ($self:ident, eval_declaration($($eval_params:tt),*)) => (
        $self.eval_declaration($($eval_params),*)
    );
    ($self:ident, $stmt_eval_func:ident($($eval_params:tt),*)) => (
        $stmt_eval_func($($eval_params),*, $self)
    );
}


#[macro_export]
macro_rules! evaluator_impl {
    (
        ast_module($($ast_module:tt)*)
        values([
            $($value_source:path => $variant:ident<$variant_type:ty>),*
        ])
        eval_statement([
            $($stmt_type:tt::$stmt_variant:ident($($args:tt)*)
                => $stmt_eval_func:ident($($params:tt),*)),*
        ])
        infix([
            $($infix_type:path => $infix_op_fn:ident),*
        ])
        prefix([
            $($prefix_type:path => $prefix_op_fn:ident),*
        ])
        postfix([
            $($postfix_type:path => $postfix_op_fn:ident),*
        ])
    ) => (

use $crate::errors::{Error, Result, ErrorKind};
use $crate::span::{Span, Spanned};
use $crate::eval::store::Store;

use $($ast_module)*::Program;
use $($ast_module)*::Block;
use $($ast_module)*::Statement;
use $($ast_module)*::Identifier;
use $($ast_module)*::Literal;
use $($ast_module)*::Expression;
use $($ast_module)*::InfixOp;
use $($ast_module)*::PrefixOp;
use $($ast_module)*::PostfixOp;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    $($variant($variant_type),)*
    Empty
}

#[allow(dead_code)]
pub fn eval<'a>(program: Program<'a>) -> Result<'a, Value> {
    Evaluator::new().eval(program)
}

pub struct Evaluator {
    #[allow(dead_code)]
    store: Store<Identifier, Value>,
}

fn eval_error<'a, T: AsRef<str>>(msg: T) -> ErrorKind<'a> {
    ErrorKind::Eval(msg.as_ref().to_string())
}
#[allow(dead_code)]
type OpResult = ::std::result::Result<Value, String>;

impl Evaluator {
    pub fn new() -> Evaluator {
        Evaluator {
            store: Store::new(),
        }
    }

    pub fn eval<'a>(&mut self, program: Program<'a>) -> Result<'a, Value> {
        self.eval_block_nopush(program)
    }

    pub fn eval_block<'a>(
        &mut self,
        sp: Span<'a>,
        block: Block<'a>
    ) -> Result<'a, Value> {
        self.store = self.store.push();
        let ret = self.eval_block_nopush(block);
        match self.store.pop() {
            Some(parent_store) => { self.store = *parent_store; },
            None => {
                return Err(Error::spanned(eval_error("invalid descoping"), sp));
            }
        }
        ret
    }

    pub fn eval_block_nopush<'a>(&mut self, mut block: Block<'a>) -> Result<'a, Value> {
        block.reverse();
        self.eval_reversed_block(block)
    }

    pub fn eval_reversed_block<'a>(&mut self, mut block: Block<'a>) -> Result<'a, Value> {
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
        ident: Identifier
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
        ident: Spanned<'a, Identifier>,
        expr: Spanned<'a, Expression<'a>>
    ) -> Result<'a, Value> {
        let assign_span = ident.span.extend_to(&expr.span);
        let rvalue = self.eval_expression(expr)?;
        match self.store.mutate(ident.item, rvalue) {
            Ok(_) => Ok(Value::Empty),
            Err(e) => Err(Error::spanned(e, assign_span))
        }
    }

    #[allow(dead_code)]
    pub fn eval_declaration<'a>(
        &mut self,
        ident: Spanned<'a, Identifier>,
        expr: Spanned<'a, Expression<'a>>
    ) -> Result<'a, Value> {
        let rvalue = self.eval_expression(expr)?;
        // TODO: handle previous declarations
        self.store.declare(ident.item, rvalue);
        Ok(Value::Empty)
    }
}


    ); // end implementation macro expression arm
}

#[macro_export]
macro_rules! evaluator {
    ($($all:tt)*) => (
        macro_preparse!(
            evaluator_impl,
            [
                ast_module,
                values,
                eval_statement,
                infix([]),
                prefix([]),
                postfix([]),
            ],
            $($all)*
        );
    );
}

#[cfg(test)]
mod tests {
    // this test just exist to make sure the macro compiles correctly and produces the appropriate
    // objects; real tests of the full lexer-parser-evaluator system will be in integration tests

    mod lexer {
        use lex::rules::{
            PTN_INT, convert_int,
        };

        lexer![
            r"\("                                   => LParen,
            r"\)"                                   => RParen,
            PTN_INT         => convert_int          => IntLiteral<i64>,
        ];
    }

    mod parser {
        use super::lexer::Token;

        group_tokens![Token: Token::LParen, Token::RParen];
        block_tokens![Token: None];
        identifier_token![Token: None];

        parser![
            token_type: Token,
            statements: [
                ExpressionStmt(expression<value>) := {expression<value>},
            ],
            literals: [
                Token::IntLiteral => Integer<i64>,
            ],
        ];
    }


    mod evaluator {

        evaluator![
            ast_module: super::parser,
            values: [
                Literal::Integer => Integer<i64>
            ],
            eval_statement: [
                Statement::ExpressionStmt(expr) => eval_expression(expr)
            ]
        ];

    }

    use self::evaluator::Value;
    use span::Spanned;

    #[test]
    fn test_simple() {
        let tokens: Vec<Spanned<lexer::Token>> = lexer::lex("5").unwrap();
        let ast = parser::parse(&tokens).unwrap();
        let value = evaluator::eval(ast).unwrap();
        assert_eq!(Value::Integer(5), value);
    }

}
