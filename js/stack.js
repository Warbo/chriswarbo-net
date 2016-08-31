/*
 * Implements a minimal concatenative ("stack") language.
 *
 * The language consists of the combinators known as "k"
 * and "cake", and quotations wrapped in "[" and "]".
 *
 * This is enough to be Turing Complete.
 *
 * Written by Chris Warburton. Released into the Public Domain.
 */

var s = (function() {

    // First we define our symbols
    // Quotations are functions from stacks
    // to stacks

    // k combinator
    var k = function(stack) {
        // [B] [A] k == A
        if (stack.length == 0) {
            stack.push(k);
            return stack;
        }
        var a = stack.pop();
        stack.pop();
        return a(stack);
    };

    // cake combinator
    var cake = function(stack) {
        // [B] [A] cake == [[B] A] [A [B]]
        if (stack.length == 0) {
            stack.push(cake);
            return stack;
        }
        var a = stack.pop();
        var b = stack.pop();
        stack.push(function(stack2) {
            stack2.push(b);
            return a(stack2);
        });
        stack.push(function(stack2) {
            stack2 = a(stack2);
            stack2.push(b);
            return stack2;
        });
        return stack;
    };

    // We read in program symbols as a string of [, ], k, c
    var read = function(input) {
        var symbols = {
            '[': 'stack.push(function(stack) {',
            ']': 'return stack; });',
            'k': 'stack = k(stack);',
            'c': 'stack = cake(stack);'
        };
        var program = '(function(stack) {';
          input = input.split('');
          while (input.length > 0) {
              program = program + symbols[input.shift()];
          }
        program = program + symbols[']'];
        return eval(program);
    };

    return read;
})();
