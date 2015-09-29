package Parser::Combinators;

use strict;
use 5.010;
our $VERSION = '0.05';

use Exporter 'import';

@Parser::Combinators::EXPORT    = qw(
        sequence 
        choice
        try
        maybe
        regex
        parens
        char
        sepBy
        oneOf
        word
        natural
        symbol
        greedyUpto
        upto
        many
        many1
        whiteSpace
        comma
        semi
        matches
        unwrap
        empty
        getParseTree
        bindP
        returnP
        $V
        );

use Data::Dumper;
our $V= 0;

# Forward declarations with prototypes

#sub symbol ($);
#sub char($);
#sub maybe($);
#sub sepBy($$);
#sub oneOf($);


# I want to write the parser using lists, because sequencing is the most common operation.
# So I need a function to generate the actual parser from the lists
# which is actually a sequence of parsers
# The first arg is a list ref, the second arg is an optional code ref to process the returned list of matches
sub sequence_ORIG {
    (my $plst, my $proc)=@_;
    my $gen = sub { (my $str)=@_;
		say "* sequence( '$str' )" if $V;
        my $matches=[];
        my $st=1;
        my $str2='';
        my $ms=undef; # Why not []?
        for my $p (@{$plst}) {
            if (ref($p) eq 'CODE') {
                ($st, $str, $ms)=$p->($str);
            } elsif (ref($p) eq 'HASH') {
                my %hp=%{$p};
                (my $k, my $pp) = each %hp;
                ($st, $str, my $mms)=$pp->($str);
                $ms = {$k => $mms};
            } else { # assuming it's ARRAY
                my $p2 = sequence($p);
                ($st, $str, $ms)=$p2->($str)
            }
            if (!$st) {
                return (0,$str,[]);
            } 	
            push @{$matches},$ms;# if scalar @{ $ms };
        }
        if (defined($proc)) {
            if( ref($proc) eq 'CODE') {
                return (1,$str,$proc->($matches));
            } else {
                say 'TROUBLE: <',show($plst),'><',show($proc),'>' if $V;
                return (1,$str,$matches);
            }
        } else {
            return (1,$str,$matches)
        }
    };
    return $gen;
}

sub foldl {
    (my $f, my $acc, my $ls)=@_;
    for my $elt (@{$ls}) {
        $acc = $f->($acc,$elt);
    }
    return $acc;
}

sub sequence {
    (my $plst, my $proc)=@_;
    my $gen = sub { (my $str)=@_;
		say "* sequence( '$str' )" if $V;
        my $f = sub { (my $acc, my $p) = @_;
            (my $st1, my $str1, my $matches) = @{ $acc };
            (my $st2, my $str2, my $ms) = apply($p,$str1);
            if ($st2*$st1==0) {
                return [0,$str1,[]];
            } else {	
                return [1,$str2,[ @{$matches},$ms]];
            }
        };     
        (my $status, $str, my $matches) = @{  foldl($f, [1,$str,[]],$plst) };
        if ($status == 0) {
            return (0,$str,[]);
        } elsif (defined($proc)) {
            if( ref($proc) eq 'CODE') {
                return (1,$str,$proc->($matches));
            } else {
                say 'TROUBLE: <',show($plst),'><',show($proc),'>' if $V;
                return (1,$str,$matches);
            }
        } else {
            return (1,$str,$matches)
        }
    };
    return $gen;
}

sub sequence_noproc {
    (my $plst )=@_;
    my $gen = sub { (my $str)=@_;
		say "* sequence( '$str' )" if $V;
        my $f = sub { (my $acc, my $p) = @_;
            (my $st1, my $str1, my $matches) = @{ $acc };
            (my $st2, my $str2, my $ms) = do {
                if (ref($p) eq 'CODE') {
                    $p->($str1);
                } elsif (ref($p) eq 'HASH') {
                    my %hp=%{$p};
                    (my $k, my $pp) = each %hp;
                    (my $st, my $str, my $mms)=$pp->($str1);
                    my $ms = {$k => $mms};
                    ($st, $str, $ms)
                } else { # assuming it's ARRAY
                    my $p2 = sequence($p);
                    $p2->($str1)
                }
            };
            if ($st2*$st1==0) {
                return [0,$str1,[]];
            } else {	
                return [1,$str2,[ @{$matches},$ms]];
            }
        };     
        (my $status, $str, my $matches) = @{  foldl($f, [1,$str,[]],$plst) };
        if ($status == 0) {
            return (0,$str,[]);
        } else {
            return (1,$str,$matches)
        }
    };
    return $gen;
} 
# In the best tradition, bind() and return()
sub bindP {
    (my $p1,my $p2) =@_;
    my $gen = sub {(my $str1) =@_;
        say "* bindP( \'$str1\' )" if $V;
        my $matches=undef;
        (my $st1,my $str2,my $m1) = $p1->( $str1 );
        push @{$matches},$m1;
        if ($st1) {
            say "bind: p1( $str1 ) matched,[$m1] try p2( $str2 )";
            (my $st2,my $str3, my $m2) = $p2->( $str2 );
           if(ref($m2) eq 'ARRAY' ){
                $matches =[ @{$matches},@{$m2} ];
            } elsif (defined $m2) {
                push @{$matches},$m2;
            }
            return ($st2,$str3,$matches);
        } else {        
            return (0,$str1,undef);
        }
    };
    return $gen;
}

# Only we can't call it 'return' so let's call it enter :-)
sub returnP {
    sub {
        (my $str) = @_;
        return (0,$str,undef);
    }
}

# Choice: try every parser in the list until one succeeds or return fail.  '<|>' in Parsec
# FIXME: Prototype does not guarantee that parens can be omitted. Should make it binary for that.
sub choice ($$;@) {
    my @parsers=@_;
    my $gen = sub {	(my $str)= @_;
		say "* choice('$str')" if $V;
        for my $p (@parsers) {
            my $status=0; 
            ($status, $str, my $matches) = apply($p,$str);
            if ($status) {
                say "choice: remainder => <$str>" if $V;
                say "choice: matches => [".show($matches)."]" if $V;
                return ($status, $str, $matches);
            }
        }
        return (0, $str, []);
    };
    return $gen;
}
# Normally, when a parser parses a string, it removes the portion that matched. If you want to keep the string, wrap the parser in try()
sub try {
    (my $p)=@_;
    my $gen = sub {
        (my $str)=@_;
		say "* try( '$str' )" if $V;
        (my $status, my $rest, my $matches)=$p->($str);
        if ($status) {
            say "try: remainder => <$rest>" if $V;
            say "try: matches => [".show($matches)."]" if $V;
            return (1, $rest, $matches);
        } else {
            say "try: match failed => <$str>" if $V;
            return (0, $str, $matches);
        }
    };
    return $gen;
}

# maybe() is like try() but always succeeds
# it returns the matches and the consumed string or the orig string and no matches
sub maybe { (my $p)=@_;
    my $gen = sub { (my $str)=@_;
		say "* maybe('$str')" if $V;       
        (my $status, my $rest, my $matches)=apply($p,$str);
        if ($status) {
            say "maybe matches: [".show($matches)."]" if $V;
            return (1, $rest, $matches);
        } else {
            say "maybe: no matches for <$str>" if $V;
            return (1, $str, undef);
        }
    };
    return $gen;
}


sub parens { (my $p)= @_;
    my $gen = sub {	(my $str)=@_;
		say "* parens($str)" if $V;
        my $matches=undef;
        (my $status, my $str3, my $ch)=char('(')->($str);
        if ($status==1) {
            my $str4 = $str3; $str4=~s/\s*//;
            (my $st,my $str4s,$matches)=$p->($str4); 
            say "parens: remainder => <$str4s>" if $V;
            say "parens: matches => [".show($matches)."]" if $V;
            $status*=$st;
            if ($status==1) {
                (my $st, my $str5, my $ch)=char(')')->($str4s);
#                if ($st==1) {
#                    $str=~s/\s*//;
#                }
                $status*=$st;
                if ($status==1) { # OK!
                    my $str6 = $str5; $str6=~s/^\s*//;
                    say "parens: remainder => <$str5>" if $V;
                    say "parens: matches => ".show($matches)."" if $V;
                    return (1,$str6, $matches);
                } else { # parse failed on closing paren
                    return (0,$str5, $matches);
                }
            } else { # parse failed on $ref
                return (0,$str4, $matches);
            }
        } else { # parse failed on opening paren
            return (0,$str3,undef);
        }
    };
    return $gen;
}

sub char { (my $ch)=@_;
    my $gen =  sub { (my $str)=@_;
        say "* char('$ch', '$str')" if $V;
        if (substr($str,0,1) eq $ch) {
            say "char: matched \'$ch\' " if $V;
			say "char: remainder <".substr($str,1).">" if $V;
            return (1,substr($str,1),$ch);
        } else {
            return (0,$str,undef);
        }
    };
    return $gen;
}

sub sepBy ($$) { (my $sep, my $p)=@_;
    my $gen = sub {	(my $str)=@_;
        my $matches=[];
		say "* sepBy('$sep', '$str')" if $V;
        (my $status,my $str1,my $m)=$p->($str);
        if ($status) {
            push @{$matches},$m;		
            say "sepBy: remainder => <$str1>" if $V;
            ($status,my $str2,$m)=char($sep)->($str1); 
            while( $status ) {
                my $str2s=$str2;$str2s=~s/^\s*//;
                (my $st,my $str3,$m)=$p->($str2s);
                push @{$matches},$m;
                ($status,$str2,$m)=char($sep)->($str3); 
            }
            say "sepBy matches => [".show($matches)."]" if $V;
            return (1, $str2, $matches);
        } else { # first match failed. 
            return (0,$str1,undef);
        }
    };
    return $gen;
}
# This is a lexeme parser, so it skips trailing whitespace
# Should be called "identifier" I think
sub word {
    my $gen = sub {	(my $str)=@_;
		say "* word( '$str' )" if $V;
        if(
                $str=~/^(\w+)/ 
          ) {
            my $m=$1;
            my $matches=$m;
            $str=~s/^$m\s*//;
            say "word: remainder => <$str>" if $V;
            say "word: matches => [$matches]" if $V;
            return (1,$str, $matches);
        } else {
            say "word: match failed => <$str>" if $V;
            return (0,$str, undef); # assumes $status is 0|1, $str is string, $matches is [string]
        }
    };
    return $gen;
}
sub identifier {
    word();
}   

# matches an unsigned integer
sub natural {
    my $gen = sub {	(my $str)=@_;
        say "* natural( '$str' )" if $V;
        my $status=0;
        if(
                $str=~/^(\d+)/ 
          ) {
            my $m=$1;
            my $matches=$m;
            $status=1;
            $str=~s/^$m\s*//;
            say "natural: remainder => <$str>" if $V;
            say "natural: matches => [$matches]" if $V;
            return ($status,$str, $matches);
        } else {
            say "natural: match failed => <$str>" if $V;
            return ($status,$str, undef); # assumes $status is 0|1, $str is string, $matches is [string]
        }
    };
    return $gen;
}

# As in Parsec, parses a literal and removes trailing whitespace
sub symbol ($) { (my $lit_str) = @_;
    $lit_str=~s/(\W)/\\$1/g; 
    my $gen = sub {	(my $str)=@_;
        say "* symbol( '$lit_str', '$str' )" if $V;
        my $status=0;
        if(
                $str=~/^\s*$lit_str\s*/ 
          ) {
            my $m=$1;
            my $matches=$lit_str;
            $status=1;
            $str=~s/^\s*$lit_str\s*//;
            say "symbol: remainder => <$str>" if $V;
            say "symbol: matches => [$matches]" if $V;
            return ($status,$str, $matches);
        } else {
            say "symbol: match failed => <$str>" if $V;
            return ($status,$str, undef); 
        }
    };
    return $gen;
}

# This parser parses anything up to the last occurence ofa given literal and trailing whitespace
sub greedyUpto {
    (my $lit_str) = @_;
    $lit_str=~s/(\W)/\\$1/g; 
    my $gen = sub {	
        (my $str)=@_;
        say "* greedyUpto( \'$lit_str\', \'$str\' )" if $V;
        if(
                $str=~/^(.*)\s*$lit_str\s*/ 
          ) {
            my $m=$1;
            $m=~s/\s*$//;
            my $matches= $m eq '' ? undef : $m;
            $str=~s/^.*$lit_str\s*//;
            say "greedyUpto: remainder => <$str>" if $V;
            say "greedyUpto: matches => [$matches]" if $V;
            return (1,$str, $matches);
        } else {
            say "greedyUpto: match failed => <$str>" if $V;
            return (0,$str, undef); 
        }
    };
    return $gen;
}

# This parser parses anything up to the last occurence of a given literal and trailing whitespace
sub upto {
    (my $lit_str )= @_;
    $lit_str=~s/(\W)/\\$1/g; 
    my $gen = sub {	
        (my $str)=@_;
        say "upto1 \'$lit_str\': <$str>" if $V;
        if(
                $str=~/^(.*?)\s*$lit_str\s*/ 
          ) {
            my $m=$1;
            my $matches= $m eq '' ? undef : $m;
            $str=~s/^.*?$lit_str\s*//;
            say "upto: remainder => <$str>" if $V;
            say "upto: matches => [$matches]" if $V;
            return (1,$str, $matches);
        } else {
            say "upto: match failed => <$str>" if $V;
            return (0,$str, undef); 
        }
    };
    return $gen;
}


# `many`, as in Parsec, parses 0 or more the specified parsers
sub many {
    (my $parser) = @_;
    my $gen = sub {
        (my $str)=@_;
        print "* many( '$str' )\n" if $V;
        (my $status,$str,my $m)=$parser->($str);
        if ($status) {
            my $matches = [$m];
            while( $status==1 ) {
                ($status,$str,$m)=$parser->($str);
                push @{$matches},$m;
            }
            print "many: remainder => <$str>\n" if $V;
            print "many: matches => [".show($matches)."]\n" if $V;
            return (1, $str, $matches);
        } else { # first match failed. 
            print "many: first match failed => <$str>\n" if $V;
            return (1, $str,undef);
        }
    };
    return $gen;
}


# `many1`, as in Parsec, parses 1 or more the specified parsers
sub many1 {
    (my $parser) = @_;
    my $gen = sub {
        (my $str)=@_;
        my $matches=[];
        say "* many( '$str' )" if $V;
        (my $status,$str,my $m)=$parser->($str);
        if ($status) {
            push @{$matches},$m;		
            while( $status==1 ) {
                (my $status,$str,$m)=$parser->($str);
                push @{$matches},$m;
            }
            say "many: remainder => <$str>" if $V;
            say "many: matches => [".show($matches)."]" if $V;
        } else { # first match failed. 
            say "many: first match failed => <$str>" if $V;
            return (0, $str,undef);
        }
        return (1, $str, $matches);
    };
    return $gen;
}

sub comma {
    my $gen = sub { (my $str) = @_;
		say "* comma( '$str' )" if $V;
        my $st = ($str=~s/^\s*,\s*//);
        if ($st) {
            say "comma: match" if $V;
        } else {
            say "comma: match failed" if $V;
        }
        return ($st, $str, undef);
    };
    return $gen;
}

sub semi {
    my $gen = sub { (my $str) = @_;
		say "* semi( '$str' )" if $V;
        my $st = ($str=~s/^\s*;\s*//);
        return ($st, $str, undef);
    };
    return $gen;
}



# strip leading whitespace, always success
sub whiteSpace {
    my $gen = sub { (my $str)=@_;
        say "* whiteSpace( \'$str\' )" if $V;
        $str=~s/^(\s*)//;	
        my $m=$1;
        return (1,$str,$m)
    };
    return $gen;
}

sub oneOf { (my $patt_lst) = @_;
    my $gen = sub { (my $str)= @_;
		say "* oneOf([".join('|',@{$patt_lst})."],'$str')" if $V;
        for my $p (@{$patt_lst}) {
            (my $status, $str, my $matches)= symbol($p)->($str);
            if ($status) {
                say "choice: remainder => <$str>" if $V;
                say "choice: matches => [".show($matches)."]" if $V;
                return (1, $str, $matches);
            }
        }
        return (0, $str, undef);
    };
    return $gen;
}

# Enough rope: this parser will parse whatever the regex is, stripping trailing whitespace
sub regex { (my $regex_str) = @_;
    my $gen = sub {	(my $str)=@_;
        say "* regex( '/$regex_str/', '$str' )" if $V;
        my $matches=undef;
        if(
                $str=~s/($regex_str)\s*//
          ) {
            my $m=$1;
            $matches=$m;
            say "regex: remainder => <$str>" if $V;
            say "regex: matches => [$matches]" if $V;
            return (1,$str, $matches);
        } else {
            say "regex: match failed => <$str>" if $V;
        }
        return (0,$str, $matches); # assumes $status is 0|1, $str is string, $matches is [string]
    };
    return $gen;
}

sub apply { (my $p, my $str) = @_;

    if (ref($p) eq 'CODE') {
        return $p->($str);
    } elsif (ref($p) eq 'ARRAY') {
        return sequence($p)->($str);
    } elsif (ref($p) eq 'HASH') {
        my %hp = %{$p};
        (my $k, my $pp) = each %hp;
        (my $status, my $str2, my $mms)=$pp->($str);
        my $matches = {$k => $mms};
        return ($status, $str2, $matches);
    } else {
        die show($p);
    }
}


sub matches {
	return @{$_[0]};
}

sub unwrap {
	(my $elt_in_array)=@_;
	my $elt = shift @{$elt_in_array};
		return $elt;
} 

sub empty {
	(my $elt_in_array)=@_;
	return (@{$elt_in_array} ) ? 0 : 1;
}

# This function returns labeled items in the parse tree.
# It is rather aggressive in removing unlabeled items
sub get_tree_as_lists { (my $list) = @_;
    my $hlist=[];
    for my $elt (@{$list}) {
        if (ref($elt) eq 'ARRAY' and scalar @{$elt}>0) { # non-empty list
            push @{ $hlist }, get_tree_as_lists($elt);
        } elsif (ref($elt) eq 'HASH') { # hash: need to process the rhs of the pair
            (my $k, my $v) = each %{$elt};
            if (ref($v) ne 'ARRAY') { # not an array => wrap in array and redo
                 push @{$hlist}, {$k => $v};
            } elsif (@{$v}==1) { # a single-elt array
                push @{$hlist}, {$k => $v->[0]};
            } else { 
                my $pv =[
                    map {
                        if (ref($_) eq 'ARRAY') {
                            get_tree_as_lists($_) 
                        } elsif ( ref($_) eq 'HASH') {
                            get_tree_as_lists([$_]) 
                        } elsif (defined $_) { $_ } 
                    } @{$v} 
                ];
                push @{$hlist}, {$k => $pv};
            }
        } 
    }
    return scalar @{$hlist}==1 ? $hlist->[0] : $hlist;
}

sub is_list_of_objects { (my $mlo) =$_;
    if (ref($mlo) eq 'ARRAY') {
        my @tmlo=@{$mlo};
    my @l=grep { ref($_) ne 'HASH' } @tmlo;
    return scalar(@l)?0:1;
    } else {
        return 0;
    }
}

sub l2m { (my $hlist) =@_;
        my $hmap = {};  
        my @hmap_vals = map {  (my $k, my $v)=%{$_}; $v } @{$hlist};
        my @hmap_keys = map {  (my $k, my $v)=%{$_}; $k } @{$hlist};
        my @hmap_rvals = map { is_list_of_objects($_) ? l2m($_) : $_ } @hmap_vals;
#        my @hmap_keys_vals = map {  each %{$_} } @{$hlist}
        for my $k ( @hmap_keys ) {
            my $rv = shift @hmap_rvals;
            $hmap->{$k}=$rv;
        }
#        my %{$hmap} = @hmap_keys_vals;
        return $hmap;
}

            
        
sub getParseTree { (my $m) =@_;
    return l2m(get_tree_as_lists($m));
}

sub run {
    (my $p, my $str) = @_;
    (my $st, my $rest, my $m) = apply($p,$str);
    getParseTree($m);
}

sub show {  (my $data)=@_;
    $Data::Dumper::Indent=0;
    $Data::Dumper::Terse =1;    
    return Dumper($data);
}

1;

__END__

=encoding utf-8

=head1 NAME

Parser::Combinators - A library of building blocks for parsing text

=head1 SYNOPSIS

  use Parser::Combinators;

  my $parser = < a combination of the parser building blocks from Parser::Combinators >
  (my $status, my $rest, my $matches) = $parser->($str);
  my $parse_tree = getParseTree($matches);


=head1 DESCRIPTION

Parser::Combinators is a library of parser building blocks ('parser combinators'), inspired by the Parsec parser combinator library in Haskell
(http://legacy.cs.uu.nl/daan/download/parsec/parsec.html).
The idea is that you build a parsers not by specifying a grammar (as in yacc/lex or Parse::RecDescent), but by combining a set of small parsers that parse
well-defined items.

=head2 Usage

Each parser in this library , e.g. C<word> or C<symbol>, is a function that returns a function (actually, a closure) that parses a string. You can combine these parsers by using special
parsers like C<sequence> and C<choice>. For example, a JavaScript variable declaration 

     var res = 42;

could be parsed as:

    my $p =
        sequence [
            symbol('var'),
            word,
            symbol('='),
            natural,
            semi
        ]

if you want to express that the assignment is optional, i.e. C< var res;> is also valid, you can use C<maybe()>:

    my $p =
        sequence [
            symbol('var'),
            word,
            maybe(
                sequence [
                   symbol('='),
                   natural
                   ]
            ),
            semi
        ]

If you want to parse alternatives you can use C<choice()>. For example, to express that either of the next two lines are valid:

    42
    return(42)

you can write

    my $p = choice( number, sequence [ symbol('return'), parens( number ) ] )

This example also illustrates the `parens()` parser to parse anything enclosed in parenthesis

=head2 Provided Parsers

The library is not complete in the sense that not all Parsec combinators have been implemented. Currently, it contains:

        whiteSpace : parses any white space, always returns success. 

        * Lexeme parsers (they remove trailing whitespace):

        word : (\w+)
        natural : (\d+)
        symbol : parses a given symbol, e.g. symbol('int')
		comma : parses a comma
        semi : parses a semicolon
        
        char : parses a given character

        * Combinators:

        sequence( [ $parser1, $parser2, ... ], $optional_sub_ref )
        choice( $parser1, $parser2, ...) : tries the specified parsers in order
        try : normally, the parser consums matching input. try() stops a parser from consuming the string
        maybe : is like try() but always reports success
        parens( $parser ) : parser '(', then applies $parser, then ')'
        many( $parser) : applies $parser zero or more times
        many1( $parser) : applies $parser one or more times
        sepBy( $separator, $parser) : parses a list of $parser separated by $separator
        oneOf( [$patt1, $patt2,...]): like symbol() but parses the patterns in order

        * Dangerous: the following parsers take a regular expression, so you can mix regexes and other combinators ...                                       

        upto( $patt )
        greedyUpto( $patt)
        regex( $patt)

=head2 Labeling

You can label any parser in a sequence using an anonymous hash, for example:

    sub type_parser {	
		sequence [
        {Type =>	word},
        maybe parens choice(
                {Kind => natural},
						sequence [
							symbol('kind'),
							symbol('='),
                            {Kind => natural}
						] 
					)        
		] 
    }

Applying this parser returns a tuple as follows:
   
  my $str = 'integer(kind=8), '
  (my $status, my $rest, my $matches) = type_parser($str);

Here,C<$status> is 0 if the match failed, 1 if it succeeded.  C<$rest> contains the rest of the string. 
The actual matches are stored in the array $matches. As every parser returns its resuls as an array ref, 
C<$matches> contains the concrete parsed syntax, i.e. a nested array of arrays of strings. 

    show($matches) ==> [{'Type' => 'integer'},['kind','\\=',{'Kind' => '8'}]]

You can remove the unlabeled matches and convert the raw tree into nested hashes using C<getParseTree>:

  my $parse_tree = getParseTree($matches);

    show($parse_tree) ==> {'Type' => 'integer','Kind' => '8'}

=head2 A more complete example

I wrote this library because I needed to parse argument declarations of Fortran-95 code. Some examples of valid declarations are:

      integer(kind=8), dimension(0:ip, -1:jp+1, kp) , intent( In ) :: u, v,w
      real, dimension(0:7) :: f 
      real(8), dimension(0:7,kp) :: f,g 

I want to extract the type and kind, the dimension and the list of variable names. For completeness I'm parsing the `intent` attribute as well.
The parser is a sequence of four separate parsers C<type_parser>, C<dim_parser>, C<intent_parser> and C<arglist_parser>.
All the optional fields are wrapped in a C<maybe()>.


    my $F95_arg_decl_parser =    
    sequence [
    	whiteSpace,
        {TypeTup => &type_parser},
	    maybe(
		    sequence [
			    comma,
                &dim_parser
	    	], 
    	),
	    maybe(
    		sequence [
	    		comma,
		    	&intent_parser
    		], 
	    ),
        &arglist_parser
	];

    # where

    sub type_parser {	
		sequence [
        {Type =>	word},
        maybe parens choice(
                {Kind => natural},
						sequence [
							symbol('kind'),
							symbol('='),
                            {Kind => natural}
						] 
					)        
		] 
    }

    sub dim_parser {
		sequence [
			symbol('dimension'),
        {Dim => parens sepBy(',', regex('[^,\)]+')) }
		] 
    }

    sub intent_parser {
	    sequence [
            symbol('intent'),
         {Intent => parens word}
		] 
    }

    sub arglist_parser {
        sequence [
        	symbol('::'),
            {Vars => sepBy(',',&word)}
        ]
    }    

Running the parser and calling C<getParseTree()> on the first string results in 

    {
    'TypeTup' => {
                'Type' => 'integer',
                'Kind' => '8'
            },
    'Dim' => ['0:ip','-1:jp+1','kp'],
    'Intent' => 'In',
    'Vars' => ['u','v','w']
    }

See the test fortran95_argument_declarations.t for the source code.    

=head3 No Monads?!

As this library is inspired by a monadic parser combinator library from Haskell, I have also implemented C<bindP()> and C<returnP()> for those who like monads ^_^
So instead of saying 

    my $pp = sequence [ $p1, $p2, $p3 ]

you can say
   
    my $pp = bindP( 
        $p1, 
        sub { (my $x) =@_;
            bindP( 
                $p2,  
                sub {(my $y) =@_;
                bindP(
                    $p3,
                    sub { (my $z) = @_;
                        returnP->($z);
                        }
                    )->($y)
                }
                )->($x);
            }
        );

which is obviously so much better :-)

=head1 AUTHOR

Wim Vanderbauwhede E<lt>Wim.Vanderbauwhede@gmail.comE<gt>

=head1 COPYRIGHT

Copyright 2013- Wim Vanderbauwhede

=head1 LICENSE

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=head1 SEE ALSO

- The original Parsec library: L<http://legacy.cs.uu.nl/daan/download/parsec/parsec.html> and L<http://hackage.haskell.org/package/parsec>

=cut

