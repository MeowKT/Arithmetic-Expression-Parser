import Foundation

protocol Calculatable: Numeric {
    init?(_ string: String)
    static func / (lhs: Self, rhs: Self) -> Self
    static func % (lhs: Self, rhs: Self) -> Self
    static func < (lhs: Self, rhs: Self) -> Bool
    static func div(lhs: Self, rhs: Self) throws -> Self
    static func mod(lhs: Self, rhs: Self) throws -> Self
    static func fact(val: Self) -> Self
    static func pow(base a: Self, power b: Self) throws -> Self
}

extension Int: Calculatable {
    static func fact(val: Self) -> Self {
        return (1...val).reduce(1, *)
    }
    
    static func pow(base a: Self, power b: Self) throws -> Self {
        return try Int(Double.pow(base: Double(a), power: Double(b)))
    }
    
    static func div(lhs: Self, rhs: Self) throws -> Self {
        guard rhs != 0 else {
            throw ArithmeticError.divideByZero
        }
        return lhs / rhs
    }
    
    static func mod(lhs: Self, rhs: Self) throws -> Self {
        guard rhs != 0 else {
            throw ArithmeticError.divideByZero
        }
        return lhs % rhs
    }
}

extension Double: Calculatable {
    static func % (lhs: Self, rhs: Self) -> Self {
        return lhs - Double(Int(lhs / rhs)) * rhs
    }
    
    static func fact(val: Self) -> Self {
        return Double((1...Int(val)).reduce(1, *))
    }
    
    static func div(lhs: Self, rhs: Self) throws -> Self {
        guard rhs != 0.0 else {
            throw ArithmeticError.divideByZero
        }
        return lhs / rhs
    }
    
    static func mod(lhs: Self, rhs: Self) throws -> Self {
        guard rhs != 0 else {
            throw ArithmeticError.divideByZero
        }
        return lhs % rhs
    }
    
    static func pow(base a: Self, power b: Self) throws -> Self {
        guard b > 0 || b == 0 && a != 0 || b < 0 && a != 0 else {
            throw ArithmeticError.powError("Range of acceptable values is violated")
        }
        var x = a
        var n = abs(Int(b))
        var ans = 1.0
        while n > 0 {
            if n % 2 != 0 {
                ans *= x
                n -= 1
            }
            x *= x
            n /= 2
        }
        return b >= 0 ? ans : 1.0 / ans;
    }
}

struct Operator<Num: Calculatable>: CustomStringConvertible {
    enum Associativity {
        case left
        case right
    }
    let name: String
    let precedence: Int
    let countArgs: Int
    let assoc: Associativity
    let f: ([Num]) throws -> Num
    var description: String {
        return self.name
    }
}

enum Token<Num: Calculatable>: CustomStringConvertible {
    case value(Num)
    case `operator`(Operator<Num>)

    var description: String {
        switch self {
        case .value(let num): return "\(num)"
        case .operator(let op): return op.description
        }
    }
}

func defaultOperators<Num: Calculatable>() -> [Operator<Num>] {
    [
        Operator(name: "(", precedence: 0, countArgs: 0, assoc: .left, f: {  (_: [Num]) in return Num.zero }),
        Operator(name: ")", precedence: 0, countArgs: 0, assoc: .left, f: {  (_: [Num]) in return Num.zero }),
        Operator(name: "+", precedence: 10, countArgs: 2, assoc: .left, f: { $0[0] + $0[1] }),
        Operator(name: "-", precedence: 10, countArgs: 2, assoc: .left, f: { $0[0] - $0[1] }),
        Operator(name: "*", precedence: 20, countArgs: 2, assoc: .left, f: { $0[0] * $0[1] }),
        Operator(name: "/", precedence: 20, countArgs: 2, assoc: .left, f: { try Num.div(lhs: $0[0], rhs: $0[1]) }),
        Operator(name: "%", precedence: 20, countArgs: 2, assoc: .left, f: { try Num.mod(lhs: $0[0], rhs: $0[1]) }),
        Operator(name: "abs", precedence: 30, countArgs: 1, assoc: .right, f: { $0[0] < Num.zero ? -1 * $0[0] : $0[0] }),
        Operator(name: "negate", precedence: 30, countArgs: 1, assoc: .right, f: { -1 * $0[0] }),
        Operator(name: "^", precedence: 40, countArgs: 2, assoc: .right, f: { try Num.pow(base: $0[0], power: $0[1]) }),
        Operator(name: "!", precedence: 50, countArgs: 1, assoc: .right, f: { Num.fact(val: $0[0]) }),
    ]
}

enum ArithmeticError: Error, CustomStringConvertible {
    case divideByZero
    case powError(String)
    
    var description: String {
        switch self {
        case .divideByZero:
            return "Divide by zero detected"
        case .powError(let s):
            return "Pow error. \(s)"
        }
    }
}

enum EvaluationError: Error, CustomStringConvertible {
    case invalidToken(token: String)
    case arityError
    case invalidUnaryOperator(op: String)
    case pairBracketError

    var description: String {
        switch self {
        case .invalidToken(token: let token):
            return "Invalid token: \"\(token)\""
        case .arityError:
            return "arity error"
        case .invalidUnaryOperator(let op):
            return "Invalid unary operator \(op)"
        case .pairBracketError:
            return "No pair bracket"
        }
    }
}

enum InputError: Error, CustomStringConvertible {
    case invalidMode(String)
    case invalidExpression(String)

    var description: String {
        switch self {
        case .invalidMode(let s):
            return "Invalid mode at Input. \(s)"
        case .invalidExpression(let s):
            return "Invalid expression at Input. \(s)"
        }
    }
}

extension Optional {
    func unwrap(or error: Error) throws -> Wrapped {
        guard let wrapped = self else {
            throw error
        }
        return wrapped
    }
}


func matches(for regex: String, in text: String) -> [String] {
    do {
        let regex = try NSRegularExpression(pattern: regex)
        let results = regex.matches(in: text, range: NSRange(text.startIndex..., in: text))
        return results.map {
            guard let rng = Range($0.range, in: text) else {
                fatalError()
            }
            return String(text[rng])
        }
    } catch {
        print("invalid regex: \(regex)")
        return []
    }
}

func stringToTokens<Num: Calculatable>(from text: String, with regular: String, operators ops: [Operator<Num>] = defaultOperators()) throws -> [Token<Num>] {
    let operators: [String: Operator<Num>] = Dictionary(uniqueKeysWithValues: ops.map { ($0.name, $0) })
    let tokens: [Token<Num>] = try matches(for: regular, in: text).map {
        try (Num($0).map(Token.value) ?? operators[$0].map(Token.operator)).unwrap(or: EvaluationError.invalidToken(token: $0))
    }
    return tokens
}

func evalReversePolishNotation<Num: Calculatable>(from expression: String) throws -> Num {
    return try evalReversePolishNotation(from: stringToTokens(from: expression, with: "([*^+!/\\-)(])|([0-9]+)"))
}

func evalReversePolishNotation<Num: Calculatable>(from rpn: [Token<Num>]) throws -> Num {
    let valStack: [Num] = try rpn.reduce(into: [Num]()) { (valStack, token) in
          switch token {
          case .value(let num):
              valStack.append(num)
          case .operator(let op):
              var args: [Num] = []
              for _ in 0..<op.countArgs {
                  guard let x = valStack.popLast() else {
                      throw EvaluationError.arityError
                  }
                  args.append(x)
              }
              try valStack.append(op.f(args.reversed()))
          }
      }
      
      guard let result = valStack.first, valStack.count == 1 else {
          throw EvaluationError.arityError
      }
      return result
}

func eval<Num: Calculatable>(_ input: String, operators ops: [Operator<Num>] = defaultOperators()) throws -> Num {
    let opNames = ops.filter { $0.name != ")" && $0.name != "(" && $0.name != "!"}.map { $0.name }
    let toUnary: [String: String] = [
        "+" : "abs",
        "-" : "negate"
    ]
    
    // let's make unary operators names different with binary operators and reduce to string again
    var lastOperator = true
    let expr: String = try matches(for: "([*^+!/\\-)(])|([0-9]+)", in: input).map { (element: String) -> String in
        if (opNames.contains(element)) {
            defer {
                lastOperator = true
            }
            return try lastOperator ? toUnary[element].unwrap(or: EvaluationError.invalidUnaryOperator(op: element)) : element
        } else {
            if (element != "(") {
                lastOperator = false
            }
            return element
        }
    }.reduce("", {$0 + " " + $1})
    
    let tokens: [Token<Num>] = try stringToTokens(from: expr, with: "([*^+!/\\-)(])|([0-9a-z]+)")

    
    let rpnExt: (rpn: [Token<Num>], opStack: [Operator<Num>]) = try tokens.reduce(into: (rpn: [], opStack: [])) { (acc, token) in
        switch token {
        case .value:
            acc.rpn.append(token)
        case .operator(let op):
            switch op.name {
            case "(":
                acc.opStack.append(op)
            case ")":
                while let topOp = acc.opStack.last, topOp.name != "(" {
                    acc.rpn.append(.operator(topOp))
                    acc.opStack.removeLast()
                }
                guard acc.opStack.count > 0 else {
                    throw EvaluationError.pairBracketError
                }
                acc.opStack.removeLast()
            default:
                while let topOp = acc.opStack.last, (op.assoc == .left ?
                                                    topOp.precedence >= op.precedence:
                                                    topOp.precedence > op.precedence) {
                    acc.rpn.append(.operator(topOp))
                    acc.opStack.removeLast()
                }
                acc.opStack.append(op)
            }
        }
    }
    
    let rpn = rpnExt.rpn + rpnExt.opStack.reversed().map(Token.operator)
    return try evalReversePolishNotation(from: rpn)
}

func parse<Num: Calculatable>(fromCommandLine cond: Bool) throws -> Num {
    switch cond {
    case true:
        let modes = CommandLine.arguments.filter({$0.starts(with: "--")})
        guard modes.count == 1 else {
            throw InputError.invalidMode("Not 1 operator at input. Use --prefix/--infix.")
        }
        let expression = CommandLine.arguments.dropFirst().filter({!$0.starts(with: "--")})
        guard expression.count == 1 else {
            throw InputError.invalidExpression("You need to write only 1 expression in commandline argument.")
        }
        switch modes[0] {
        case "--infix":
            return try eval(expression[0])
        case "--prefix":
            return try evalReversePolishNotation(from: expression[0])
        default:
            throw InputError.invalidMode("We have no mode \(modes[0]) in parser.")
        }
    case false:
        print("Please write your expression in infix form")
        if let s = readLine() {
            return try eval(s)
        }
        throw InputError.invalidExpression("No expression in input")
    }
}

print(try parse(fromCommandLine: false) as Double)
