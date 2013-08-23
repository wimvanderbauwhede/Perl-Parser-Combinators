# NAME

Parser::Combinators - A library of building blocks for parsing, similar to Haskell's Parsec

# SYNOPSIS

    use Parser::Combinators;

    my $parser = < a combination of the parser building blocks from Parser::Combinators >
    (my $status, my $rest, my $matches) = $parser->($str);
    my $parse_tree = getParseTree($matches);



# DESCRIPTION

Parser::Combinators is a simple parser combinator library inspired by the Parsec parser combinator library in Haskell.
It is not complete (i.e. not all Parsec combinators have been implemented), I have just implemented what I needed:

        whiteSpace : parses any white space, always returns success. I

        Lexeme parsers (they remove trailing whitespace):
        word : (\w+)
        number : (\d+)
        symbol : parses a given symbol, e.g. symbol('int')
		comma : parses a comma
                

        char : parses a given character

        Combinators:
        sequence( [ $parser1, $parser2, ... ], $optional_sub_ref )
        choice( $parser1, $parser2, ...) : tries the specified parsers in order
        try : normally, the parser consums matching input. try() stops a parser from consuming the string
        maybe : is like try() but always reports success
        parens( $parser ) : parser '(', then applies $parser, then ')'
        many( $parser) : applies $parser zero or more times
        sepBy( $separator, $parser) : parses a list of $parser separated by $separator
        oneOf( [$patt1, $patt2,...]): like symbol() but parses the patterns in order

        Dangerous: the following parsers take a regular expression                                       
        upto( $patt )
        greedyUpto( $patt)
        regex( $patt)



As there is no Haskell-style syntactic sugar in Perl, I use the sequence() combinator where in Haskell you would use the do-notation.
sequence() takes a ref to a list of parsers and optionally a code ref to a sub that can manipulate the result before returning it.

Also, you can label any parser in a sequence using an anonymous hash, for example:

    sub type_parser {	
		sequence [
        {Type =>	word},
        maybe parens choice(
                {Kind => number},
						sequence [
							symbol('kind'),
							symbol('='),
                            {Kind => number}
						] 
					)        
		] 
    }

Applying this parser returns a tuple as follows:
   

    my $str = 'integer(kind=8), '
    (my $status, my $rest, my $matches) = type_parser($str);

Here,\`$status\` is 0 if the match failed, 1 if it succeeded.  \`$rest\` contains the rest of the string. 
The actual matches are stored in the array $matches. As every parser returns its resuls as an array ref, 
$matches contains the concrete parsed syntax, i.e. a nested array of arrays of strings. 

    Dumper($matches) ==> [{'Type' => ['integer']},[['kind'],['\\='],{'Kind' => ['8']}]]

You can extract only
the labeled matches using \`getParseTree\`:

    my $parse_tree = getParseTree($matches);

    Dumper($parse_tree) ==> [{'Type' => 'integer'},{'Kind' => '8'}]



PS: I have also implemented bind() and enter() (as 'return' is reserved) for those who like monads ^\_^

# AUTHOR

Wim Vanderbauwhede <Wim.Vanderbauwhede@gmail.com>

# COPYRIGHT

Copyright 2013- Wim Vanderbauwhede

# LICENSE

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

# SEE ALSO
