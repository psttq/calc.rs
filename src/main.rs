use clap::Parser;
use nom::{
    IResult,
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, digit1, space0},
    combinator::{cut, map, map_res, opt, recognize},
    error::{Error, context},
    sequence::{delimited, pair, preceded, separated_pair, tuple},
};

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Number(f64),
    BinaryOp {
        op: BinaryOpType,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Constant(ConstantType),
    Function {
        ftype: FunctionType,
        expr: Box<Expr>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum FunctionType {
    SQRT,
    SIN,
    COS,
    TG,
    CTG,
    LN,
    EXP,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ConstantType {
    PI,
    E,
}

#[derive(Debug, PartialEq, Clone)]
pub enum BinaryOpType {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
}

fn parse_constant(input: &str) -> IResult<&str, Expr> {
    map(
        alt((
            map(tag("pi"), |_| ConstantType::PI),
            map(tag("e"), |_| ConstantType::E),
        )),
        |constant: ConstantType| Expr::Constant(constant),
    )(input)
}

fn parse_function_type(input: &str) -> IResult<&str, FunctionType> {
    alt((
        map(tag("sqrt"), |_| FunctionType::SQRT),
        map(tag("cos"), |_| FunctionType::COS),
        map(tag("sin"), |_| FunctionType::SIN),
        map(tag("tg"), |_| FunctionType::TG),
        map(tag("ctg"), |_| FunctionType::CTG),
        map(tag("ln"), |_| FunctionType::LN),
        map(tag("exp"), |_| FunctionType::EXP),
    ))(input)
}

fn parse_function(input: &str) -> IResult<&str, Expr> {
    map(
        preceded(space0, pair(parse_function_type, parse_parenthesized)),
        |res| Expr::Function {
            ftype: res.0,
            expr: Box::new(res.1),
        },
    )(input)
}

fn parse_simple_number(input: &str) -> IResult<&str, Expr> {
    context(
        "number",
        map_res(recognize(preceded(space0, digit1)), |s: &str| {
            s.parse::<f64>().map(Expr::Number)
        }),
    )(input)
}

fn parse_float_number(input: &str) -> IResult<&str, Expr> {
    map_res(
        recognize(preceded(space0, separated_pair(digit1, char('.'), digit1))),
        |s: &str| s.parse::<f64>().map(Expr::Number),
    )(input)
}

fn parse_scientific_notation(input: &str) -> IResult<&str, Expr> {
    map_res(
        recognize(tuple((
            alt((parse_float_number, parse_simple_number)),
            char('e'),
            opt(alt((char('+'), char('-')))),
            digit1,
        ))),
        |s: &str| s.parse::<f64>().map(Expr::Number),
    )(input)
}

fn parse_parenthesized(input: &str) -> IResult<&str, Expr> {
    context(
        "parenthesized expression",
        delimited(
            preceded(space0, char('(')),
            cut(parse_add_sub),
            preceded(space0, char(')')),
        ),
    )(input)
}

fn parse_term(input: &str) -> IResult<&str, Expr> {
    context(
        "term",
        alt((
            parse_function,
            parse_parenthesized,
            parse_scientific_notation,
            parse_float_number,
            parse_simple_number,
            parse_constant,
        )),
    )(input)
}

fn parse_add_sub_op(input: &str) -> IResult<&str, BinaryOpType> {
    context(
        "add/sub operator",
        alt((
            map(char('+'), |_| BinaryOpType::Add),
            map(char('-'), |_| BinaryOpType::Sub),
        )),
    )(input)
}

fn parse_mul_div_op(input: &str) -> IResult<&str, BinaryOpType> {
    context(
        "mul/div operator",
        alt((
            map(char('*'), |_| BinaryOpType::Mul),
            map(char('/'), |_| BinaryOpType::Div),
        )),
    )(input)
}

fn parse_power(input: &str) -> IResult<&str, Expr> {
    let (mut input, mut left) = parse_term(input)?;

    loop {
        match preceded(space0, char('^'))(input) {
            Ok((input_after_op, _)) => match preceded(space0, parse_term)(input_after_op) {
                Ok((input_after_right, right)) => {
                    left = Expr::BinaryOp {
                        op: BinaryOpType::Pow,
                        left: Box::new(left),
                        right: Box::new(right),
                    };
                    input = input_after_right;
                }
                Err(_e) => {
                    return Err(nom::Err::Error(Error::new(
                        input_after_op,
                        nom::error::ErrorKind::MapRes,
                    )));
                }
            },
            Err(nom::Err::Error(_)) | Err(nom::Err::Failure(_)) => break,
            Err(e) => return Err(e),
        }
    }

    Ok((input, left))
}

fn parse_mul_div(input: &str) -> IResult<&str, Expr> {
    let (mut input, mut left) = parse_power(input)?;

    loop {
        match preceded(space0, parse_mul_div_op)(input) {
            Ok((input_after_op, op)) => match preceded(space0, parse_power)(input_after_op) {
                Ok((input_after_right, right)) => {
                    left = Expr::BinaryOp {
                        op,
                        left: Box::new(left),
                        right: Box::new(right),
                    };
                    input = input_after_right;
                }
                Err(_e) => {
                    return Err(nom::Err::Error(Error::new(
                        input_after_op,
                        nom::error::ErrorKind::MapRes,
                    )));
                }
            },
            Err(nom::Err::Error(_)) | Err(nom::Err::Failure(_)) => break,
            Err(e) => return Err(e),
        }
    }

    Ok((input, left))
}
fn parse_add_sub(input: &str) -> IResult<&str, Expr> {
    let (mut input, mut left) = parse_mul_div(input)?;

    loop {
        match preceded(space0, parse_add_sub_op)(input) {
            Ok((input_after_op, op)) => match preceded(space0, parse_mul_div)(input_after_op) {
                Ok((input_after_right, right)) => {
                    left = Expr::BinaryOp {
                        op,
                        left: Box::new(left),
                        right: Box::new(right),
                    };
                    input = input_after_right;
                }
                Err(_e) => {
                    return Err(nom::Err::Error(Error::new(
                        input_after_op,
                        nom::error::ErrorKind::MapRes,
                    )));
                }
            },
            Err(nom::Err::Error(_)) | Err(nom::Err::Failure(_)) => break,
            Err(e) => return Err(e),
        }
    }

    Ok((input, left))
}

pub fn parse(input: &str) -> Result<Expr, String> {
    match parse_add_sub(input) {
        Ok(("", expr)) => Ok(expr),
        Ok((rest, _)) => Err(format!("Unexpected trailing characters: '{}'", rest)),
        Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {
            let msg = match e.code {
                nom::error::ErrorKind::Tag => "Expected operator",
                nom::error::ErrorKind::Digit => "Invalid number",
                _ => "Syntax error",
            };
            Err(format!("{} at '{}'", msg, e.input))
        }
        Err(nom::Err::Incomplete(_)) => Err("Incomplete input".to_string()),
    }
}

pub fn check_on_inf(input: f64) -> Result<f64, String> {
    if f64::is_finite(input) {
        Ok(input)
    } else {
        Err("Value overflow".to_string())
    }
}

pub fn eval(expr: &Expr, use_degrees: bool) -> Result<f64, String> {
    match expr {
        Expr::Number(val) => Ok(*val),
        Expr::Constant(constant) => match constant {
            ConstantType::PI => Ok(std::f64::consts::PI),
            ConstantType::E => Ok(std::f64::consts::E),
        },
        Expr::Function { ftype, expr } => {
            let res = eval(expr, use_degrees)?;
            let mut angle_res = res;
            if use_degrees {
                angle_res = f64::to_radians(res);
            }
            match ftype {
                FunctionType::TG => check_on_inf(f64::tan(angle_res)),
                FunctionType::CTG => check_on_inf(1.0 / f64::tan(angle_res)),
                FunctionType::COS => check_on_inf(f64::cos(angle_res)),
                FunctionType::SIN => check_on_inf(f64::sin(angle_res)),
                FunctionType::EXP => check_on_inf(f64::exp(res)),
                FunctionType::LN => check_on_inf(f64::ln(res)),
                FunctionType::SQRT => check_on_inf(f64::sqrt(res)),
            }
        }
        Expr::BinaryOp { op, left, right } => {
            let left_val = eval(left, use_degrees)?;
            let right_val = eval(right, use_degrees)?;
            match op {
                BinaryOpType::Add => check_on_inf(left_val + right_val),
                BinaryOpType::Sub => check_on_inf(left_val - right_val),
                BinaryOpType::Mul => check_on_inf(left_val * right_val),
                BinaryOpType::Div => {
                    if right_val == 0.0 {
                        Err("Division by zero".to_string())
                    } else {
                        check_on_inf(left_val / right_val)
                    }
                }
                BinaryOpType::Pow => check_on_inf(left_val.powf(right_val)),
            }
        }
    }
}

fn parse_eval(input: &str, use_degrees: bool) {
    match parse(input) {
        Ok(expr) => match eval(&expr, use_degrees) {
            Ok(result) => println!("Result: {}", result),
            Err(e) => println!("Evaluation error: {}", e),
        },
        Err(e) => println!("Parse error: {}", e),
    }
}

#[derive(Parser, Debug)]
struct Cli {
    expr: String,
    #[arg(default_value = "radians", long)]
    angle_unit: String,
}
fn main() {
    let args = Cli::parse();
    let use_degrees: bool = match args.angle_unit.as_str() {
        "radians" => false,
        "degrees" => true,
        _ => {
            println!("Error: Unknown angle unit");
            return;
        }
    };
    println!("{:?}", parse(args.expr.as_str()));
    parse_eval(args.expr.as_str(), use_degrees);
}

#[cfg(test)]
mod parser_and_eval_tests {
    use super::*;

    #[test]
    fn parse_simple_number_test() {
        let result = parse_simple_number("123");
        assert_eq!(result, Ok(("", Expr::Number(123f64))));

        let result = parse_simple_number("123");
        assert_eq!(result, Ok(("", Expr::Number(123f64))));
    }

    #[test]
    fn parse_float_number_test() {
        let result = parse_float_number("123.123");
        assert_eq!(result, Ok(("", Expr::Number(123.123f64))));
    }

    #[test]
    fn parse_function_test() {
        let result = parse_function("sin(4)");
        assert_eq!(
            result,
            Ok((
                "",
                Expr::Function {
                    ftype: FunctionType::SIN,
                    expr: Box::new(Expr::Number(4.0))
                }
            ))
        );

        let result = parse_function("cos(4)");
        assert_eq!(
            result,
            Ok((
                "",
                Expr::Function {
                    ftype: FunctionType::COS,
                    expr: Box::new(Expr::Number(4.0))
                }
            ))
        );

        let result = parse_function("tg(4)");
        assert_eq!(
            result,
            Ok((
                "",
                Expr::Function {
                    ftype: FunctionType::TG,
                    expr: Box::new(Expr::Number(4.0))
                }
            ))
        );

        let result = parse_function("ctg(4)");
        assert_eq!(
            result,
            Ok((
                "",
                Expr::Function {
                    ftype: FunctionType::CTG,
                    expr: Box::new(Expr::Number(4.0))
                }
            ))
        );

        let result = parse_function("exp(4)");
        assert_eq!(
            result,
            Ok((
                "",
                Expr::Function {
                    ftype: FunctionType::EXP,
                    expr: Box::new(Expr::Number(4.0))
                }
            ))
        );

        let result = parse_function("ln(4)");
        assert_eq!(
            result,
            Ok((
                "",
                Expr::Function {
                    ftype: FunctionType::LN,
                    expr: Box::new(Expr::Number(4.0))
                }
            ))
        );

        let result = parse_function("sqrt(4)");
        assert_eq!(
            result,
            Ok((
                "",
                Expr::Function {
                    ftype: FunctionType::SQRT,
                    expr: Box::new(Expr::Number(4.0))
                }
            ))
        );
    }

    #[test]
    fn parse_term_numbers_test() {
        let result = parse_term("123");
        assert_eq!(result, Ok(("", Expr::Number(123f64))));

        let result = parse_term("123.43");
        assert_eq!(result, Ok(("", Expr::Number(123.43))));

        let result = parse_term("1.2e-5");
        assert_eq!(result, Ok(("", Expr::Number(1.2e-5))));
    }

    #[test]
    fn parse_mul_div_test() {
        let result = parse_mul_div("2");
        assert_eq!(result, Ok(("", Expr::Number(2f64))));

        let result = parse_mul_div("2*3");
        assert_eq!(
            result,
            Ok((
                "",
                Expr::BinaryOp {
                    op: BinaryOpType::Mul,
                    left: Box::new(Expr::Number(2.0)),
                    right: Box::new(Expr::Number(3.0))
                }
            ))
        );

        let result = parse_mul_div("2/3");
        assert_eq!(
            result,
            Ok((
                "",
                Expr::BinaryOp {
                    op: BinaryOpType::Div,
                    left: Box::new(Expr::Number(2.0)),
                    right: Box::new(Expr::Number(3.0))
                }
            ))
        );
    }

    #[test]
    fn parse_power_test() {
        let result = parse_power("3^4");
        assert_eq!(
            result,
            Ok((
                "",
                Expr::BinaryOp {
                    op: BinaryOpType::Pow,
                    left: Box::new(Expr::Number(3.0)),
                    right: Box::new(Expr::Number(4.0))
                }
            ))
        );
    }

    #[test]
    fn parse_add_sub_test() {
        let result = parse_add_sub("1+2");
        assert_eq!(
            result,
            Ok((
                "",
                Expr::BinaryOp {
                    op: BinaryOpType::Add,
                    left: Box::new(Expr::Number(1.0)),
                    right: Box::new(Expr::Number(2.0))
                }
            ))
        );

        let result = parse_add_sub("1-2");
        assert_eq!(
            result,
            Ok((
                "",
                Expr::BinaryOp {
                    op: BinaryOpType::Sub,
                    left: Box::new(Expr::Number(1.0)),
                    right: Box::new(Expr::Number(2.0))
                }
            ))
        );

        let result = parse_add_sub("1+2*3");
        assert_eq!(
            result,
            Ok((
                "",
                Expr::BinaryOp {
                    op: BinaryOpType::Add,
                    left: Box::new(Expr::Number(1.0)),
                    right: Box::new(Expr::BinaryOp {
                        op: BinaryOpType::Mul,
                        left: Box::new(Expr::Number(2.0)),
                        right: Box::new(Expr::Number(3.0))
                    })
                }
            ))
        );

        let result = parse_add_sub("2*3+1");
        assert_eq!(
            result,
            Ok((
                "",
                Expr::BinaryOp {
                    op: BinaryOpType::Add,
                    right: Box::new(Expr::Number(1.0)),
                    left: Box::new(Expr::BinaryOp {
                        op: BinaryOpType::Mul,
                        left: Box::new(Expr::Number(2.0)),
                        right: Box::new(Expr::Number(3.0))
                    })
                }
            ))
        );
    }

    #[test]
    fn parse_parenthesized_test() {
        let result = parse_parenthesized("(1+2)");
        assert_eq!(
            result,
            Ok((
                "",
                Expr::BinaryOp {
                    op: BinaryOpType::Add,
                    left: Box::new(Expr::Number(1.0)),
                    right: Box::new(Expr::Number(2.0))
                }
            ))
        );
    }

    #[test]
    fn parse_term_parenthesized_test() {
        let result = parse_term("(1+2)");
        assert_eq!(
            result,
            Ok((
                "",
                Expr::BinaryOp {
                    op: BinaryOpType::Add,
                    left: Box::new(Expr::Number(1.0)),
                    right: Box::new(Expr::Number(2.0))
                }
            ))
        );
    }

    #[test]
    fn parse_mul_div_parenthesized_test() {
        let result = parse_mul_div("(1+2)*4");
        assert_eq!(
            result,
            Ok((
                "",
                Expr::BinaryOp {
                    op: BinaryOpType::Mul,
                    left: Box::new(Expr::BinaryOp {
                        op: BinaryOpType::Add,
                        left: Box::new(Expr::Number(1.0)),
                        right: Box::new(Expr::Number(2.0))
                    }),
                    right: Box::new(Expr::Number(4.0))
                }
            ))
        );
    }

    #[test]
    fn parser_errors_test() {
        let result = parse_add_sub("1/");
        assert_eq!(
            result,
            Err(nom::Err::Error(Error {
                input: "",
                code: nom::error::ErrorKind::MapRes,
            }))
        );

        let result = parse_add_sub("1+(3-");
        assert_eq!(
            result,
            Err(nom::Err::Error(Error {
                input: "(3-",
                code: nom::error::ErrorKind::MapRes,
            }))
        );

        let result = parse("1+4j");
        assert_eq!(
            result,
            Err("Unexpected trailing characters: 'j'".to_string())
        );

        let result = parse("1/a");
        assert_eq!(result, Err("Syntax error at 'a'".to_string()));
    }

    #[test]
    fn evaluator_test() {
        let result = eval(
            &Expr::BinaryOp {
                op: BinaryOpType::Add,
                left: Box::new(Expr::Number(1.0)),
                right: Box::new(Expr::Number(2.0)),
            },
            false,
        );
        assert_eq!(result, Ok(3.0));

        let result = eval(
            &Expr::BinaryOp {
                op: BinaryOpType::Mul,
                left: Box::new(Expr::Number(3.0)),
                right: Box::new(Expr::Number(4.0)),
            },
            false,
        );
        assert_eq!(result, Ok(12.0));

        let result = eval(
            &Expr::BinaryOp {
                op: BinaryOpType::Sub,
                left: Box::new(Expr::Number(3.0)),
                right: Box::new(Expr::Number(4.0)),
            },
            false,
        );
        assert_eq!(result, Ok(-1.0));

        let result = eval(
            &Expr::BinaryOp {
                op: BinaryOpType::Div,
                left: Box::new(Expr::Number(3.0)),
                right: Box::new(Expr::Number(4.0)),
            },
            false,
        );
        assert_eq!(result, Ok(3.0 / 4.0));

        let result = eval(
            &Expr::BinaryOp {
                op: BinaryOpType::Div,
                left: Box::new(Expr::Number(3.0)),
                right: Box::new(Expr::Number(0.0)),
            },
            false,
        );
        assert_eq!(result, Err("Division by zero".to_string()));

        let result = eval(
            &Expr::BinaryOp {
                op: BinaryOpType::Div,
                left: Box::new(Expr::Number(1e300)),
                right: Box::new(Expr::Number(1e-300)),
            },
            false,
        );
        assert_eq!(result, Err("Value overflow".to_string()));

        let result = eval(
            &Expr::Function {
                ftype: FunctionType::SQRT,
                expr: Box::new(Expr::Number(4.0)),
            },
            true,
        );
        assert_eq!(result, Ok(2.0));

        let result = eval(
            &Expr::Function {
                ftype: FunctionType::SIN,
                expr: Box::new(Expr::Number(90.0)),
            },
            true,
        );
        assert_eq!(result, Ok(1.0));

        let result = eval(
            &Expr::Function {
                ftype: FunctionType::COS,
                expr: Box::new(Expr::Number(0.0)),
            },
            true,
        );
        assert_eq!(result, Ok(1.0));

        let result = eval(
            &Expr::Function {
                ftype: FunctionType::TG,
                expr: Box::new(Expr::Number(0.0)),
            },
            true,
        );
        assert_eq!(result, Ok(0.0));

        let result = eval(
            &Expr::Function {
                ftype: FunctionType::CTG,
                expr: Box::new(Expr::Number(45.0)),
            },
            true,
        );
        assert_eq!(result, Ok(1.0 / f64::tan(f64::to_radians(45.0))));

        let result = eval(
            &Expr::Function {
                ftype: FunctionType::EXP,
                expr: Box::new(Expr::Number(2.0)),
            },
            true,
        );
        assert_eq!(result, Ok(f64::exp(2.0)));

        let result = eval(
            &Expr::Function {
                ftype: FunctionType::LN,
                expr: Box::new(Expr::Number(2.0)),
            },
            true,
        );
        assert_eq!(result, Ok(f64::ln(2.0)));
    }
    #[test]
    fn parse_eval_test() {
        fn make_test(input: &str) -> Result<f64, String> {
            match parse(input) {
                Ok(expr) => match eval(&expr, false) {
                    Ok(result) => Ok(result),
                    Err(e) => Err(format!("Evaluation error: {}", e)),
                },
                Err(e) => Err(format!("Parse error: {}", e)),
            }
        }
        let result = make_test("1+2/3");
        assert_eq!(result, Ok(1.0 + 2.0 / 3.0));

        let result = make_test("1+2^(1+2)");
        assert_eq!(result, Ok(9.0));

        let result = make_test("1/");
        assert_eq!(result, Err("Parse error: Syntax error at ''".to_string()));

        let result = make_test("3/0");
        assert_eq!(
            result,
            Err("Evaluation error: Division by zero".to_string())
        );
    }
}

#[cfg(test)]
mod load_tests {
    use std::time::Instant;

    const MAX_TIME_MS: u128 = 200u128;

    use super::*;

    fn parse_and_eval(input: &str, use_degrees: bool) -> Result<f64, String> {
        let expr = parse(input)?;
        let result = eval(&expr, use_degrees)?;
        Ok(result)
    }

    fn load_test(input: &str, use_degrees: bool) -> Result<bool, String> {
        let start = Instant::now();
        parse_and_eval(input, use_degrees)?;
        let duration = start.elapsed();
        if input.len() > 1000 {
            return Ok(true);
        }
        return Ok(duration.as_millis() < MAX_TIME_MS);
    }

    #[test]
    fn load_test1() {
        let input = "1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1";

        assert_eq!(load_test(input, false), Ok(true));
    }

    #[test]
    fn load_test2() {
        let input = "1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+";

        assert_eq!(
            load_test(input, false),
            Err("Syntax error at ''".to_string())
        );
    }

    #[test]
    fn load_test3() {
        let input = "sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)*sin(1)";

        assert_eq!(load_test(input, false), Ok(true));
    }

    #[test]
    fn load_test4() {
        let input = "2^2^2^2^2^2^2^2^2^2";
        assert_eq!(load_test(input, false), Ok(true));
    }

    #[test]
    fn load_test5() {
        let input = "100000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000";
        assert_eq!(load_test(input, false), Ok(true));
    }

    #[test]
    fn load_test6() {
        let input = "1^8765456789987654567";
        assert_eq!(load_test(input, false), Ok(true));
    }

    #[test]
    fn load_test7() {
        let input = "10^8765456789987654567";
        assert_eq!(load_test(input, false), Err("Value overflow".to_string()));
    }

    #[test]
    fn load_test8() {
        let input = "1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1)))))))))))))))))))))))))))))))))))))))";
        assert_eq!(load_test(input, false), Ok(true));
    }

    #[test]
    fn load_test9() {
        let input = "1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1 / (1 + 1)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))";
        assert_eq!(load_test(input, false), Ok(true));
    }

    #[test]
    fn load_test10() {
        let input = "exp(ln(exp(ln(exp(ln(exp(ln(exp(ln(exp(ln(exp(ln(exp(ln(exp(ln(exp(ln(exp(ln(exp(ln(exp(10)))))))))))))))))))))))))";
        assert_eq!(load_test(input, false), Ok(true));
    }
}
