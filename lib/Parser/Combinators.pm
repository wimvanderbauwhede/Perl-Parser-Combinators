package Parser::Combinators;

use strict;
use 5.008_005;
our $VERSION = '0.01';

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
        number
        symbol
        greedyUpto
        upto
        many
        whiteSpace
		comma
		matches
		unwrap
		empty
        getParseTree
        $V
        );

use Data::Dumper;
$Data::Dumper::Indent=0;
$Data::Dumper::Terse =1;
our $V= 0;

# I want to write the parser using lists, because sequencing is the most common operation.
# So I need a function to generate the actual parser from the lists
# which is actually a sequence of parsers
# The first arg is a list ref, the second arg is an optional code ref to process the returned list of matches
sub sequence {
    (my $plst, my $proc)=@_;
    my $gen = sub {
        (my $str)=@_;
		print "* sequence($str)\n" if $V;
        my $matches=[];
        my $st=1;
        my $str2='';
        my $ms=[];
        for my $p (@{$plst}) {
            if (ref($p) eq 'CODE') {
                ($st, $str, $ms)=$p->($str);
            } elsif (ref($p) eq 'HASH') {
#                print "HASH: ".Dumper($p)."\n";
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
                print 'TROUBLE: <',Dumper($plst),'><',Dumper($proc),'>' if $V;
                return (1,$str,$matches);
            }
        } else {
            return (1,$str,$matches)
        }
    };
    return $gen;
}

# In the best tradition, bind() and return()
sub bind {
    (my $p1,my $p2) =@_;
    my $gen = sub {(my $str1) =@_;
        my $matches=[];
        (my $st1,my $m1,my $str2) = $p1->( $str1 );
        push @{$matches},$m1;
        if ($st1==1 ) {
            (my $st2,my $m2,my $str3) = $p2->( $str2 );
            push @{$matches},$m2;
            return ($st2,$matches,$str3);
        } else {        
            return (0,[],$str1);
        }
    };
    return $gen;
}

# Only we can't call it 'return' so let's call it enter :-)
sub enter {
    (my $str) = @_;
    return (0,$str,[]);
}

# how to do 'OR' without running?
sub choice {
    my @parsers=@_;
    my $gen = sub {	(my $str)= @_;
		print "* choice('$str')\n" if $V;
        for my $p (@parsers) {
            my $status=0; my $matches=[];
            if (ref($p) eq 'CODE') {
                ($status, $str, $matches)=$p->($str);
            } elsif (ref($p) eq 'HASH') {
                my %hp = %{$p};
                (my $k, my $pp) = each %hp;
#                print $str,Dumper($k);
                ($status, $str, my $mms)=$pp->($str);
                $matches = {$k => $mms};
            } else {
                die Dumper($p);
            }
#            (my $status, $str, my $matches)=$p->($str);
            if ($status) {
                print "choice: remainder => <$str>\n" if $V;
                print "choice: matches => [".Dumper($matches)."]\n" if $V;
                return ($status, $str, $matches);
            }
        }
        return (0, $str, []);
    };
    return $gen;
}
# Finally, try()
# 
sub try {
    (my $p)=@_;
    my $gen = sub {
        (my $str)=@_;
        (my $status, my $rest, my $matches)=$p->($str);
        if ($status) {
            print "try: $status <$rest>\n" if $V;
            print "try: $status ".Dumper($matches)."\n" if $V;
            return (1, $rest, $matches);
        } else {
            print "try: $status <$str>\n" if $V;
            return (0, $str, $matches);
        }
    };
    return $gen;
}

# maybe() is like try() but always succeeds
# it returns the matches and the consumed string or the orig string and no matches
sub maybe {
    (my $p)=@_;
    my $gen = sub {
        (my $str)=@_;
		print "* maybe('$str')\n" if $V;
        (my $status, my $rest, my $matches)=$p->($str);
        if ($status) {
#            print "maybe: $status <$rest>\n" if $V;
            print "maybe matches: [".Dumper($matches)."]\n" if $V;
            return (1, $rest, $matches);
        } else {
            print "maybe: no matches for <$str>\n" if $V;
            return (1, $str, []);
        }
    };
    return $gen;
}

# Enough rope: this parser will parse whatever the regex is, stripping trailing whitespace
sub regex {
    (my $regex_str) = @_;
    my $gen = sub {	
        (my $str)=@_;
#		$str=~s/^\s+//;
        print "* regex( '/$regex_str/', '$str' )\n" if $V;
        my $matches=[];
        my $status=0;
        if(
                $str=~s/($regex_str)\s*//
          ) {
            my $m=$1;
            $matches=[$m];
            $status=1;
            print "regex: remainder => <$str>\n" if $V;
            print 'regex: matches => ['.join(';',@{$matches}),"]\n" if $V;
            return ($status,$str, $matches);
        } else {
            print "regex: match failed => <$str>\n" if $V;
#            print 'regex23:'.join(';',@{$matches}),"\n" if $V;
        }

        return ($status,$str, $matches); # assumes $status is 0|1, $str is string, $matches is [string]
    };
    return $gen;

}

sub parens {
    (my $ref)= @_;
    my $gen = sub {	(my $str)=@_;
		print "* parens($str)\n" if $V;
        my $matches=[];
#		$str=~s/^\s+//;
        (my $status, my $str, my $ch)=char('(')->($str);
#        print "parens1: $status => <$str>\n" if $V;
        if ($status==1) {
            $str=~s/\s*//;
            print "parens: <$str>\n" if $V;
            (my $st,$str,$matches)=$ref->($str); 
            print "parens: remainder => <$str>\n" if $V;
            print "parens: matches => [".Dumper($matches)."]\n" if $V;
#			push @{$matches},$m;
            $status*=$st;
            if ($status==1) {
                (my $st, $str, my $ch)=char(')')->($str);
                if ($st==1) {
                    $str=~s/\s*//;
                }
                $status*=$st;
                if ($status==1) {
# OK!
                    print "parens: matches => ".Dumper($matches)."\n" if $V;

                    return (1,$str, $matches);
                } else {
# parse failed on closing paren
                    return (0,$str, $matches);
                }

            } else {
# parse failed on $ref
                return (0,$str, $matches);
            }
        } else {
# parse failed on opening paren
            return (0,$str,[]);
        }
    };
    return $gen;
}

sub char {
#    print "char0\n" if $V;
    (my $ch)=@_;
    my $gen =  sub {
        (my $str)=@_;
        print "* char('$ch', '$str')\n" if $V;
        if (substr($str,0,1) eq $ch) {
            print "char: matched \'$ch\' \n" if $V;
			print "char: remainder <".substr($str,1).">\n" if $V;
            return (1,substr($str,1),[$ch]);
        } else {
            return (0,$str,[]);
        }
    };
    return $gen;
}

sub sepBy {
    (my $sep, my $ref)=@_;
    my $gen = sub {	(my $str)=@_;
		 print "* sepBy('$sep', '$str')\n" if $V;
        my $matches=[];
#        print "sepBy0 \'$sep\': <$str>\n" if $V;
        (my $status,$str,my $m)=$ref->($str);
        if ($status) {
            push @{$matches},$m;		
            print "sepBy: remainder => <$str>\n" if $V;
            while( do {($status,$str,$m)=char($sep)->($str); 
                    if ($status) {$str=~s/\s*//;};
                    $status==1} ) {
                (my $st,$str,$m)=$ref->($str);
                push @{$matches},$m;
#                $status*=$st;
            }
#            print "sepBy2 \'$sep\': $status => <$str>\n" if $V;
            print "sepBy matches => [".Dumper($matches)."]\n" if $V;
        } else {
# first match failed. 
            return (0,$str,[]);
        }
        return (1, $str, $matches);
    };
    return $gen;
}
# This is a lexeme parser, so it skips trailing whitespace
sub word {
    my $gen = sub {	
        (my $str)=@_;
		print "* word( '$str' )\n" if $V;
#$str=~s/^\s+//;
#        print "word1: <$str>\n" if $V;
        my $status=0;
        my $matches=[];
        if(
                $str=~/^(\w+)/ 
          ) {
            my $m=$1;
            $matches=[$m];
            $status=1;
            $str=~s/^$m\s*//;
            print "word: remainder => <$str>\n" if $V;
            print 'word: matches => ['.join(', ',@{$matches}),"]\n" if $V;
            return ($status,$str, $matches);
        } else {
            print "word: match failed => <$str>\n" if $V;
        return ($status,$str, $matches); # assumes $status is 0|1, $str is string, $matches is [string]
#            print 'word23:'.join(';',@{$matches}),"\n" if $V;
        }

    };
    return $gen;
}
# matches an unsigned integer
sub number {
    my $gen = sub {	
        (my $str)=@_;
#$str=~s/^\s+//;
        print "* number( '$str' )\n" if $V;
        my $status=0;
        my $matches=[];
        if(
                $str=~/^(\d+)/ 
          ) {
            my $m=$1;
            $matches=[$m];
            $status=1;
            $str=~s/^$m\s*//;
            print "number: remainder => <$str>\n" if $V;
            print 'number: matches => ['.join(',',@{$matches}),"]\n" if $V;
            return ($status,$str, $matches);
        } else {
            print "number: match failed => <$str>\n" if $V;
#            print 'number23:'.join(';',@{$matches}),"\n" if $V;
        }

        return ($status,$str, $matches); # assumes $status is 0|1, $str is string, $matches is [string]
    };
    return $gen;


}
# As in Parsec, parses a literal and removes trailing whitespace
sub symbol {
    (my $lit_str) = @_;
    $lit_str=~s/(\W)/\\$1/g; 

    my $gen = sub {	
        (my $str)=@_;
#		$str=~s/^\s+//;
        print "* symbol('$lit_str', '$str' )\n" if $V;
        my $status=0;
        my $matches=[];
        if(
                $str=~/^\s*$lit_str\s*/ 
          ) {
            my $m=$1;
            $matches=[$lit_str];
            $status=1;
            $str=~s/^\s*$lit_str\s*//;
            print "symbol: remainder => <$str>\n" if $V;
            print 'symbol: matches => ['.join(',',@{$matches}),"]\n" if $V;
            return ($status,$str, $matches);
        } else {
            print "symbol: match failed => <$str>\n" if $V;
#            print 'symbol23:'.join(';',@{$matches}),"\n" if $V;
        }

        return ($status,$str, $matches); # assumes $status is 0|1, $str is string, $matches is [string]
    };
    return $gen;
}

# This parser parses anything up to the last occurence ofa given literal and trailing whitespace
sub greedyUpto {
    (my $lit_str) = @_;
    print "greedyUpto $lit_str\n" if $V;    
    my $gen = sub {	
        (my $str)=@_;
        print "greedyUpto1 \'$lit_str\': <$str>\n" if $V;
        my $status=0; my $matches=[];
        if(
                $str=~/^(.*)\s*$lit_str\s*/ 
          ) {
            my $m=$1;
            $m=~s/\s*$//;
            $matches= $m eq '' ? [] : [$m];
            $status=1;
            $str=~s/^.*$lit_str\s*//;
            print "greedyUpto4 \'$lit_str\': <$str>\n" if $V;
            print "greedyUpto43 \'$lit_str\':".join(';',@{$matches}),"\n" if $V;
            return ($status,$str, $matches);
        } else {
            print "greedyUpto2 \'$lit_str\': <$str>\n" if $V;
            print "greedyUpto23 \'$lit_str\':".join(';',@{$matches}),"\n" if $V;
        }

        return ($status,$str, $matches); # assumes $status is 0|1, $str is string, $matches is [string]
    };
    return $gen;
}

# This parser parses anything up to the last occurence of a given literal and trailing whitespace
sub upto {
    (my $lit_str )= @_;
    print "upto $lit_str\n" if $V;    
    my $gen = sub {	
        (my $str)=@_;
        print "upto1 \'$lit_str\': <$str>\n" if $V;
        my $status=0; my $matches=[];
        if(
                $str=~/^(.*?)\s*$lit_str\s*/ 
          ) {
            my $m=$1;
            $matches= $m eq '' ? [] : [$m];
            $status=1;
            $str=~s/^.*?$lit_str\s*//;
            print "upto4 \'$lit_str\': <$str>\n" if $V;
            print "upto43 \'$lit_str\':".join(';',@{$matches}),"\n" if $V;
            return ($status,$str, $matches);
        } else {
            print "upto2 \'$lit_str\': <$str>\n" if $V;
            print "upto23 \'$lit_str\':".join(';',@{$matches}),"\n" if $V;
        }

        return ($status,$str, $matches); # assumes $status is 0|1, $str is string, $matches is [string]
    };
    return $gen;
}


# many , as in Parsec, parses 0 or more the specified parsers
sub many {
    (my $parser) = @_;
    my $gen = sub {
        (my $str)=@_;
        my $matches=[];
        print "many( '$str' )\n" if $V;
        (my $status,$str,my $m)=$parser->($str);
        if ($status) {
            push @{$matches},$m;		
            print "many1: $status => <$str>\n" if $V;
            while( $status==1 ) {
                (my $st,$str,$m)=$parser->($str);
                push @{$matches},$m;
            }
            print "many: remainder => <$str>\n" if $V;
            print "many: matches => [".Dumper($matches)."]\n" if $V;
        } else {
# first match failed. 
            print "many: first match failed => <$str>\n" if $V;
            return (0,$str,[]);
        }
        return (1, $str, $matches);
    };
    return $gen;
}

sub comma {
    my $gen = sub { (my $str) = @_;
		print "* comma( '$str' )\n" if $V;
        my $st = ($str=~s/^\s*,\s*//);
        return ($st, $str, []);
    };
    return $gen;
}

# strip leading whitespace, always success
sub whiteSpace {
    my $gen = sub {
        (my $str)=@_;

        my $st = ($str=~s/^\s*//);	
            print "whiteSpace: <$str>\n" if $V;

        return (1,$str,[])
    };
    return $gen;
}

sub oneOf {
    (my $patt_lst) = @_;
    my $gen = sub {
	(my $str)= @_;
		print "* oneOf([".join('|',@{$patt_lst})."],'$str')\n" if $V;
        for my $p (@{$patt_lst}) {
            (my $status, $str, my $matches)= symbol($p)->($str);
            if ($status) {
                print "choice: remainder => <$str>\n" if $V;
                print "choice: matches => [".Dumper($matches)."]\n" if $V;
                return ($status, $str, $matches);
            }
        }
        return (0, $str, []);
    };
    return $gen;
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
sub getParseTree { (my $list) = @_;
    my $hlist=[];
    for my $elt (@{$list}) {
        if (ref($elt) eq 'ARRAY' and scalar @{$elt}>0) { # non-empty list
            my $ch_hlist = getParseTree($elt);
            if (@{$ch_hlist}==1) { # single-elt array
                push @{$hlist}, $ch_hlist->[0];
            } else {
                push @{$hlist},$ch_hlist;
            }
        } elsif (ref($elt) eq 'HASH') { # hash: need to process the rhs of the pair
            (my $k, my $v) = each %{$elt};
            if (ref($v) ne 'ARRAY') { # not an array => wrap in array and redo
                my $ch_hlist = getParseTree([$v]);
                if (@{$ch_hlist}==1) { # single-elt array
                    push @{$hlist}, {$k => $ch_hlist->[0]};
                } else { # multi-elt array
                    push @{$hlist}, {$k => $ch_hlist};
                }
            } elsif (@{$v}==1) { # a single-elt array
                push @{$hlist}, {$k => $v->[0]};
            } elsif (scalar (grep {ref($_) eq 'ARRAY' 
                        and scalar @{$_} == 1} @{$v}) == scalar @{$v} ) { # $v is an array of single-elt arrays
                push @{$hlist}, {$k => [map { $_->[0] } @{$v}]};
            } else {
                my $pv = getParseTree($v);
                my $ppv = [grep {ref($_) eq 'HASH'} @{$pv} ];
                my $tppv = (@{$ppv} == 1) ? $ppv->[0] : $ppv;
                my $nelt = {$k => $tppv};
                push @{$hlist}, $nelt;
            }
        } 
    }
    return [ grep {ref($_) eq 'HASH'} @{ $hlist } ];
}

1;

__END__

=encoding utf-8

=head1 NAME

Parser::Combinators - A library of building blocks for parsing, similar to Haskell's Parsec

=head1 SYNOPSIS

  use Parser::Combinators;

  my $parser = < a combination of the parser building blocks from Parser::Combinators >
  (my $status, my $rest, my $matches) = $parser->($str);
  my $parse_tree = getParseTree($matches);


=head1 DESCRIPTION

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

Here,`$status` is 0 if the match failed, 1 if it succeeded.  `$rest` contains the rest of the string. 
The actual matches are stored in the array $matches. As every parser returns its resuls as an array ref, 
$matches contains the concrete parsed syntax, i.e. a nested array of arrays of strings. 

    Dumper($matches) ==> [{'Type' => ['integer']},[['kind'],['\\='],{'Kind' => ['8']}]]

You can extract only
the labeled matches using `getParseTree`:

  my $parse_tree = getParseTree($matches);

    Dumper($parse_tree) ==> [{'Type' => 'integer'},{'Kind' => '8'}]


PS: I have also implemented bind() and enter() (as 'return' is reserved) for those who like monads ^_^

=head1 AUTHOR

Wim Vanderbauwhede E<lt>Wim.Vanderbauwhede@gmail.comE<gt>

=head1 COPYRIGHT

Copyright 2013- Wim Vanderbauwhede

=head1 LICENSE

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=head1 SEE ALSO

=cut

