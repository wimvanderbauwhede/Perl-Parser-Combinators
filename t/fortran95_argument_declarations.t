use strict;
use Test::More  tests => 3;

use Parser::Combinators;

use Data::Dumper;
$Data::Dumper::Indent = 0;
$Data::Dumper::Terse = 1;
$Parser::Combinators::V=1;
sub test_raw {
    (my $comb, my $str, my $ref) =@_;
    my @res = $comb->($str);
    my $assertion = Dumper(@res) eq $ref;
    if (not $assertion) {
        print Dumper(@res),'<>',$ref,"\n";
    };
    return $assertion;
}

sub test_parsetree {
    (my $comb, my $str, my $ref) =@_;
    (my $st, my $rest, my $ms)= $comb->($str);
#    print STDERR Dumper($ms),"\n\n";
    my $pt= getParseTree($ms);
#     print STDERR Dumper($pt),"\n\n";

    my $assertion = Dumper($pt) eq $ref;
    if (not $assertion) {
        print STDERR '<<<'.Dumper($pt).'>>>','<>','<<<<'.$ref.'>>>>',"\n";
    };
    return $assertion;
}


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
    	symbol('::'),
        {Vars => sepBy(',',&word)}
	] 
;

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

my $str1 = '      integer(kind=8), dimension(0:ip, -1:jp+1, kp) , intent( In ) :: u, v,w';
my $str2 = '      real, dimension(0:7) :: f ';
my $str3 = '      real(8), dimension(0:7,kp) :: f,g ';

ok( test_parsetree( $F95_arg_decl_parser, $str1, "[{'TypeTup' => [{'Type' => 'integer'},{'Kind' => '8'}]},{'Dim' => ['0:ip','-1:jp+1','kp']},{'Intent' => 'In'},{'Vars' => ['u','v','w']}]" ) );
ok( test_parsetree( $F95_arg_decl_parser, $str2, "[{'TypeTup' => [{'Type' => 'real'},'']},{'Dim' => '0:7'},{'Vars' => 'f'}]" ) );
ok( test_parsetree( $F95_arg_decl_parser, $str3, "[{'TypeTup' => [{'Type' => 'real'},{'Kind' => '8'}]},{'Dim' => ['0:7','kp']},{'Vars' => ['f','g']}]" ) );


done_testing;
