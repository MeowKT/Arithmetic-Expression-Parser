"use strict";

const operators = (function () {

    function expressionInterface(constr, evaluate, toString, diff, prefix, postfix) {
        constr.prototype.evaluate = evaluate;
        constr.prototype.toString = toString;
        constr.prototype.diff = diff;
        constr.prototype.prefix = prefix;
        constr.prototype.postfix = postfix;
    }

    function Const(x) {
        this.x = x;
    }

    expressionInterface(
        Const,
        function () {
            return parseFloat(this.x)
        },
        function () {
            return this.x.toString()
        },
        function () {
            return ZERO
        },
        function () {
            return this.toString()
        },
        function () {
            return this.toString()
        }
    );

    const ZERO = new Const(0);
    const ONE = new Const(1);
    const TWO = new Const(2);

    function Variable(variable) {
        this.variable = variable;
        this.number = variables[variable];
    }

    const variables = {
        x: 0,
        y: 1,
        z: 2
    };

    expressionInterface(
        Variable,
        function (...vars) {
            return vars[this.number]
        },
        function () {
            return this.variable
        },
        function (variable) {
            return this.variable === variable ? ONE : ZERO
        },
        function () {
            return this.toString()
        },
        function () {
            return this.toString()
        }
    );

    function MultiExpression(...args) {
        this.args = args;
    }

    expressionInterface(
        MultiExpression,
        function (...elements) {
            return this.op(...this.args.map(el => el.evaluate(...elements)));
        },
        function () {
            return this.args.join(" ") + " " + this.symbol;
        },
        function (variable) {
            return this.diffRule(variable, ...this.args);
        },
        function () {
            return "(" + this.symbol + " " + this.args.map(el => el.prefix()).join(" ") + ")";
        },
        function () {
            return "(" + this.args.map(el => el.postfix()).join(" ") + " " + this.symbol + ")";
        }
    );

    function makeOperation(op, symbol, diffRule) {
        let operation = function (...args) {
            MultiExpression.call(this, ...args);
        };
        operation.prototype = Object.create(MultiExpression.prototype);
        operation.prototype.op = op;
        operation.prototype.symbol = symbol;
        operation.prototype.diffRule = diffRule;
        return operation;
    }

    const Negate = makeOperation(
        a => -a,
        "negate",
        (name, a) => new Negate(a.diff(name))
    );

    const Add = makeOperation(
        (a, b) => a + b,
        "+",
        (name, a, b) => new Add(a.diff(name), b.diff(name)));

    const Subtract = makeOperation(
        (a, b) => a - b,
        "-",
        (name, a, b) => new Subtract(a.diff(name), b.diff(name)),
    );

    const Multiply = makeOperation(
        (a, b) => a * b,
        "*",
        (name, a, b) =>
            new Add(
                new Multiply(a, b.diff(name)),
                new Multiply(a.diff(name), b))
    );

    const Mean = makeOperation(
        (...args) => args.length === 0 ? 0 : args.reduce((sum, current) => sum + current) / args.length,
        "mean",
        (name, ...args) => new Mean(...args.map(el => el.diff(name)))
    );

    const Var = makeOperation(
        (...args) => {
            let M = Mean.prototype.op(...args);
            return Mean.prototype.op(...args.map(el => (el - M) * (el - M)));
        },
        "var",
        (arg, ...operands) => new Subtract(new Mean(...operands.map(x => new Multiply(x, x))), new Multiply(new Mean(...operands), new Mean(...operands))).diff(arg)
    );

    const Divide = makeOperation(
        (a, b) => a / b,
        "/",
        (name, a, b) =>
            new Divide(
                new Subtract(
                    new Multiply(a.diff(name), b),
                    new Multiply(a, b.diff(name))),
                new Multiply(b, b))
    );

    const Gauss = makeOperation(
        (a, b, c, x) => a * Math.exp(-(x - b) * (x - b) / (c * c * 2)),
        "gauss",
        function (name, a, b, c, x) {
            let sub = new Subtract(x, b);
            return new Multiply(
                new Gauss(ONE, b, c, x),
                new Add(
                    a.diff(name),
                    new Multiply(
                        a,
                        new Negate(
                            new Divide(
                                new Multiply(
                                    sub,
                                    sub),
                                new Multiply(
                                    TWO,
                                    new Multiply(c, c)))).diff(name))))
        }
    );
    return {
        Const: Const,
        Variable: Variable,
        Add: Add,
        Subtract: Subtract,
        Multiply: Multiply,
        Divide: Divide,
        Gauss: Gauss,
        Var: Var,
        Mean: Mean,
        Negate: Negate,
        variables: variables
    }
})();

let exceptions = (function () {
    function MyException(message) {
        this.message = message;
    }
    MyException.prototype = Object.create(Error.prototype);

    function BracketException(pos) {
        this.message = `Expected closed bracket at pos: ${pos}`
    }
    BracketException.prototype = Object.create(MyException.prototype);

    function UnsupportedVariableException(pos, token) {
        this.message = `Variable : ${token} at pos: ${pos} is not supported.`
    }
    UnsupportedVariableException.prototype = Object.create(MyException.prototype);

    function UnknownSymbolException(pos, symbol) {
        this.message = `Unknown symbol ${symbol} at pos : ${pos}`
    }
    UnknownSymbolException.prototype = Object.create(MyException.prototype);

    function TokenException(pos) {
        this.message = pos > -1 ? `Expected token at pos: ${pos}` : 'Empty input';
    }
    TokenException.prototype = Object.create(MyException.prototype);

    function OperationException(pos, expected, found) {
        this.message = `Expected ${expected} at pos ${pos}, but found: ${found}`;
    }
    OperationException.prototype = Object.create(MyException.prototype);

    function UnsupportedArgumentCountException(pos, op, cnt) {
        this.message = op.length > 0 ? `Operator ${op} at pos: ${pos} can't apply ${cnt} arguments` : `There is no operator at pos: ${pos}`;
    }
    UnsupportedArgumentCountException.prototype = Object.create(MyException.prototype);

    function UnsupportedArgumentException(pos) {
        this.message = `Invalid arguments at pos ${pos}`;
    }
    UnsupportedArgumentException.prototype = Object.create(MyException.prototype);

    function MissFunctionException(pos) {
        this.message = `Expected function at pos ${pos}`;
    }
    MissFunctionException.prototype = Object.create(MyException.prototype);

    return {
        MyException: MyException,
        BracketException: BracketException,
        TokenError: TokenException,
        OperationError: OperationException,
        UnsupportedArgumentsError: UnsupportedArgumentCountException,
        UnsupportedVariableException: UnsupportedVariableException,
        UnknownSymbolException: UnknownSymbolException,
        MissFunctionException: MissFunctionException
    }
})();

const parsers = (function () {

    // Import
    const Const = operators.Const;
    const Variable = operators.Variable;
    const Add = operators.Add;
    const Subtract = operators.Subtract;
    const Multiply = operators.Multiply;
    const Divide = operators.Divide;
    const Gauss = operators.Gauss;
    const Negate = operators.Negate;
    const Mean = operators.Mean;
    const Var = operators.Var;
    const variables = operators.variables;
    const MyException = exceptions.MyException;
    const BracketException = exceptions.BracketException;
    const OperationError = exceptions.OperationError;
    const UnsupportedArgumentsError = exceptions.UnsupportedArgumentsError;
    const TokenError = exceptions.TokenError;
    const UnsupportedVariableException = exceptions.UnsupportedVariableException;
    const UnknownSymbolException = exceptions.UnknownSymbolException;
    const MissFunctionException = exceptions.MissFunctionException;


    const operations = {
        "gauss": Gauss,
        "+": Add,
        "-": Subtract,
        "*": Multiply,
        "/": Divide,
        "n": Negate,
        "m": Mean,
        "v": Var
    };

    const prefixToOperation = {
        "+" : "+",
        "-" : "-",
        "*" : "*",
        "/" : "/",
        "n" : "negate",
        "m" : "mean",
        "v" : "var"
    };

    let AbstractParser = (function () {
        let source;
        function BaseParser(str) {
            this.ch = "";
            this.prch = "";
            source = str;
            this.ind = 0;
        }

        BaseParser.prototype.nextChar = function () {
            this.prch = this.ch;
            if (this.ind < source.length) {
                this.ch = source[this.ind++];
            } else {
                this.ch = '\0';
            }
        };
        BaseParser.prototype.expect = function (value) {
            for (let el of value) {
                if (this.ch !== el) {
                    throw new OperationError(this.ind - 1, el, this.ch);
                } else {
                    this.nextChar()
                }
            }
        };
        BaseParser.prototype.between = function (from, to) {
            return from <= this.ch && this.ch <= to;
        };
        BaseParser.prototype.test = function (el) {
            if (this.ch === el) {
                this.nextChar();
                return true;
            } else {
                return false;
            }
        };
        return {
            BaseParser : BaseParser
        }
    })();

    function ExpressionParser(expression, isPrefix) {
        this.funcIndex = isPrefix;
        AbstractParser.BaseParser.call(this, expression);
    }
    ExpressionParser.prototype = Object.create(AbstractParser.BaseParser.prototype);

    function parse(expression) {
        let stack = [];
        for (let token of expression.trim().split(/\s+/)) {
            if (token in operations) {
                let operation = operations[token];
                stack.push(new operation(...stack.splice(-operation.prototype.op.length)));
            } else if (token in operations.variables) {
                stack.push(new Variable(token));
            } else {
                stack.push(new Const(parseFloat(token)));
            }
        }
        return stack.pop();
    }

    ExpressionParser.prototype.parseExpression = function() {
        this.nextChar();
        let res = this.parseToken();
        this.skipWhitespace();
        if (this.ch !== '\0') {
            throw new UnknownSymbolException(this.ind - 1, this.ch);
        }
        return res;
    };

    ExpressionParser.prototype.hasNextToken = function() {
        this.skipWhitespace();
        return (this.ch === '(' ||
            (this.prch === '(' || this.prch === ')' || this.prch === ' ') && this.ch !== ')');
    };

    ExpressionParser.prototype.parseOperation = function() {
        let s = [];
        let cnt = 0;
        while (this.hasNextToken()) {
            let token = this.parseToken();
            if (typeof token === "function") {
                cnt++;
            }
            s.push(token);
        }
        let op = this.funcIndex === 0 ? s.pop() : s.shift();
        if (typeof op !== "function") {
            throw new MissFunctionException(this.ind);
        }
        if (cnt > 1) {
            throw new MyException(`Invalid arguments at pos ${this.ind}`);
        }
        if (op.prototype.op.length === 0 || s.length === op.prototype.op.length) {
            return new op(...s);
        } else {
            throw new UnsupportedArgumentsError(this.ind - 1, this.funcIndex !== -1 ? op.prototype.symbol : "", s.length);
        }
    };

    ExpressionParser.prototype.parseToken = function() {
        this.skipWhitespace();
        if (this.ch === '(') {
            this.nextChar();
            let ans = this.parseOperation();
            this.skipWhitespace();
            if (this.ch !== ')') {
                throw new BracketException(this.ind - 1);
            }
            this.nextChar();
            return ans;
        } else if (this.test('-')) {
            if (this.between('0', '9')) {
                return this.parseConst('-');
            } else {
                return Subtract;
            }
        } else if (this.between('0', '9')) {
            return this.parseConst();
        } else if (this.ch in operations) {
            let op = operations[this.ch];
            this.expect(prefixToOperation[this.ch]);
            return op;
        } else if (this.between('a', 'z')) {
            return this.parseVariable();
        } else {
            throw new TokenError(this.ind - 1);
        }
    };

    ExpressionParser.prototype.parseConst = function(isNeg) {
        let s = [isNeg];
        while (this.ch !== '\0' && this.between('0', '9')) {
            s.push(this.ch);
            this.nextChar();
        }
        return new Const(parseFloat(s.join("")));
    };

    ExpressionParser.prototype.parseVariable = function() {
        let s = [];
        while (this.ch !== '\0' && this.between('a', 'z')) {
            s.push(this.ch);
            this.nextChar();
        }
        let variable = s.join("");
        if (!(variable in variables)) {
            throw new UnsupportedVariableException(this.ind - 1, variable);
        }
        return new Variable(variable);
    };

    ExpressionParser.prototype.skipWhitespace = function() {
        while (this.ch !== '\0' && this.ch === " ") {
            this.nextChar();
        }
    };

    return {
        Parser: ExpressionParser,
    }
})();

const Const = operators.Const;
const Variable = operators.Variable;
const Add = operators.Add;
const Subtract = operators.Subtract;
const Multiply = operators.Multiply;
const Divide = operators.Divide;
const Gauss = operators.Gauss;
const Negate = operators.Negate;
const Mean = operators.Mean;
const Var = operators.Var;

function parsePrefix(expr) {
    let parser = new parsers.Parser(expr, 1);
    return parser.parseExpression();
}
function parsePostfix(expr) {
    let parser = new parsers.Parser(expr, 0);
    return parser.parseExpression();
}

