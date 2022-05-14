"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[309],{3905:function(n,e,r){r.d(e,{Zo:function(){return o},kt:function(){return u}});var t=r(7294);function a(n,e,r){return e in n?Object.defineProperty(n,e,{value:r,enumerable:!0,configurable:!0,writable:!0}):n[e]=r,n}function p(n,e){var r=Object.keys(n);if(Object.getOwnPropertySymbols){var t=Object.getOwnPropertySymbols(n);e&&(t=t.filter((function(e){return Object.getOwnPropertyDescriptor(n,e).enumerable}))),r.push.apply(r,t)}return r}function i(n){for(var e=1;e<arguments.length;e++){var r=null!=arguments[e]?arguments[e]:{};e%2?p(Object(r),!0).forEach((function(e){a(n,e,r[e])})):Object.getOwnPropertyDescriptors?Object.defineProperties(n,Object.getOwnPropertyDescriptors(r)):p(Object(r)).forEach((function(e){Object.defineProperty(n,e,Object.getOwnPropertyDescriptor(r,e))}))}return n}function l(n,e){if(null==n)return{};var r,t,a=function(n,e){if(null==n)return{};var r,t,a={},p=Object.keys(n);for(t=0;t<p.length;t++)r=p[t],e.indexOf(r)>=0||(a[r]=n[r]);return a}(n,e);if(Object.getOwnPropertySymbols){var p=Object.getOwnPropertySymbols(n);for(t=0;t<p.length;t++)r=p[t],e.indexOf(r)>=0||Object.prototype.propertyIsEnumerable.call(n,r)&&(a[r]=n[r])}return a}var s=t.createContext({}),h=function(n){var e=t.useContext(s),r=e;return n&&(r="function"==typeof n?n(e):i(i({},e),n)),r},o=function(n){var e=h(n.components);return t.createElement(s.Provider,{value:e},n.children)},c={inlineCode:"code",wrapper:function(n){var e=n.children;return t.createElement(t.Fragment,{},e)}},g=t.forwardRef((function(n,e){var r=n.components,a=n.mdxType,p=n.originalType,s=n.parentName,o=l(n,["components","mdxType","originalType","parentName"]),g=h(r),u=a,m=g["".concat(s,".").concat(u)]||g[u]||c[u]||p;return r?t.createElement(m,i(i({ref:e},o),{},{components:r})):t.createElement(m,i({ref:e},o))}));function u(n,e){var r=arguments,a=e&&e.mdxType;if("string"==typeof n||a){var p=r.length,i=new Array(p);i[0]=g;var l={};for(var s in e)hasOwnProperty.call(e,s)&&(l[s]=e[s]);l.originalType=n,l.mdxType="string"==typeof n?n:a,i[1]=l;for(var h=2;h<p;h++)i[h]=r[h];return t.createElement.apply(null,i)}return t.createElement.apply(null,r)}g.displayName="MDXCreateElement"},606:function(n,e,r){r.r(e),r.d(e,{frontMatter:function(){return l},contentTitle:function(){return s},metadata:function(){return h},toc:function(){return o},default:function(){return g}});var t=r(7462),a=r(3366),p=(r(7294),r(3905)),i=["components"],l={},s="Parser Combinators",h={unversionedId:"fp/parser-combinators",id:"fp/parser-combinators",title:"Parser Combinators",description:"Introduction",source:"@site/../grox-docs/target/mdoc/fp/parser-combinators.md",sourceDirName:"fp",slug:"/fp/parser-combinators",permalink:"/grox/docs/fp/parser-combinators",editUrl:"https://github.com/facebook/docusaurus/tree/main/packages/create-docusaurus/templates/shared/../grox-docs/target/mdoc/fp/parser-combinators.md",tags:[],version:"current",frontMatter:{},sidebar:"docs",previous:{title:"Parsing Expression",permalink:"/grox/docs/book/parsing-expression"},next:{title:"Resources",permalink:"/grox/docs/resources"}},o=[{value:"Introduction",id:"introduction",children:[],level:2},{value:"Parser representation",id:"parser-representation",children:[],level:2},{value:"Parser with cats",id:"parser-with-cats",children:[{value:"Simple Parsers",id:"simple-parsers",children:[],level:3},{value:"Mapping Output",id:"mapping-output",children:[],level:3},{value:"Combinging parsers",id:"combinging-parsers",children:[],level:3},{value:"Repeating parsers",id:"repeating-parsers",children:[{value:"Parsers with empty output",id:"parsers-with-empty-output",children:[],level:4}],level:3},{value:"Error Handling",id:"error-handling",children:[],level:3},{value:"Backtrack",id:"backtrack",children:[],level:3},{value:"Soft",id:"soft",children:[],level:3}],level:2},{value:"Resources",id:"resources",children:[],level:2}],c={toc:o};function g(n){var e=n.components,r=(0,a.Z)(n,i);return(0,p.kt)("wrapper",(0,t.Z)({},c,r,{components:e,mdxType:"MDXLayout"}),(0,p.kt)("h1",{id:"parser-combinators"},"Parser Combinators"),(0,p.kt)("h2",{id:"introduction"},"Introduction"),(0,p.kt)("p",null,"Trong b\xe0i n\xe0y, m\xecnh s\u1ebd tr\xecnh b\xe0y v\u1ec1 parser combinators m\u1ed9t k\u1ef9 thu\u1eadt th\u01b0\u1eddng \u0111\u01b0\u1ee3c s\u1eed d\u1ee5ng trong Functional Programming khi gi\u1ea3i quy\u1ebft c\xe1c b\xe0i to\xe1n parsing. N\u1ebfu ch\xfang ta coi Parser l\xe0 m\u1ed9t function c\xf3 input l\xe0 String v\xe0 output l\xe0 m\u1ed9t structured data; th\xec parser combinator l\xe0 m\u1ed9t higher-order function nh\u1eadn m\u1ed9t ho\u1eb7c nhi\u1ec1u parser v\xe0 k\u1ebft h\u1ee3p ch\xfang l\u1ea1i th\xe0nh m\u1ed9t parser m\u1edbi. \xdd t\u01b0\u1edfng \u1edf \u0111\xe2y l\xe0 ch\xfang ta c\xf3 th\u1ec3 d\xf9ng c\xe1c parser \u0111\u01a1n gi\u1ea3n v\xe0 g\u1ed9p ch\xfang l\u1ea1i b\u1eb1ng c\xe1c parser combinator \u0111\u1ec3 gi\u1ea3i quy\u1ebft c\xe1c b\xe0i to\xe1n ph\u1ee9c t\u1ea1p h\u01a1n."),(0,p.kt)("p",null,"M\u1ed9t s\u1ed1 v\xed d\u1ee5 b\xe0i to\xe1n v\u1ec1 parsing nh\u01b0: compiler c\u1ea7n parse source code th\xe0nh ",(0,p.kt)("a",{parentName:"p",href:"https://en.wikipedia.org/wiki/Abstract_syntax_tree"},"Abstract Syntax Tree/AST")," ho\u1eb7c m\u1ed9t chu\u1ed7i c\xe1c token (n\u1ebfu compiler t\xe1ch ri\xeang ",(0,p.kt)("a",{parentName:"p",href:"/grox/docs/book/scanning"},"scanning"),"); json parser s\u1ebd parser json string th\xe0nh c\xe1c data class."),(0,p.kt)("p",null,"Sau \u0111\xe2y m\xecnh s\u1ebd s\u1eed d\u1ee5ng ng\xf4n ng\u1eef ",(0,p.kt)("a",{parentName:"p",href:"https://docs.scala-lang.org/scala3/getting-started.html"},"Scala 3")," v\xe0 th\u01b0 vi\u1ec7n ",(0,p.kt)("a",{parentName:"p",href:"https://github.com/typelevel/cats-parse"},"cats-parse")," \u0111\u1ec3 gi\u1ea3i th\xedch v\u1ec1 parser combinator nh\u01b0ng tr\u01b0\u1edbc h\u1ebft h\xe3y b\u1eaft \u0111\u1ea7u b\u1eb1ng vi\u1ec7c kh\xe1m ph\xe1 type c\u1ee7a parser",(0,p.kt)("sup",{parentName:"p",id:"fnref-1"},(0,p.kt)("a",{parentName:"sup",href:"#fn-1",className:"footnote-ref"},"1")),"."),(0,p.kt)("h2",{id:"parser-representation"},"Parser representation"),(0,p.kt)("p",null,"Nh\u01b0 ch\xfang ta \u0111\xe3 n\xf3i \u1edf tr\xean, parser l\xe0 m\u1ed9t function nh\u1eadn String v\xe0 tr\u1ea3 v\u1ec1 m\u1ed9t k\u1ebft qu\u1ea3 m\xe0 ch\xfang ta \u0111ang mong \u0111\u1ee3i. T\u1eeb \u0111\xf3 ch\xfang ta c\xf3 th\u1ec3 \u0111\u1ecbnh ngh\u0129a parser nh\u01b0 sau:"),(0,p.kt)("pre",null,(0,p.kt)("code",{parentName:"pre",className:"language-scala"},"type Parser[A] = String => A\n")),(0,p.kt)("p",null,"Type nh\u01b0 v\u1eady th\xec \u0111\u01a1n gi\u1ea3n nh\u01b0ng ch\u01b0a \u0111\u1ee7. \u0110i\u1ec1u \u0111\u1ea7u ti\xean ch\xfang ta th\u1ea5y l\xe0 kh\xf4ng ph\u1ea3i l\xfac n\xe0o c\u0169ng c\xf3 th\u1ec3 parse th\xe0nh c\xf4ng. Khi input kh\xf4ng \u0111\xfang c\xfa ph\xe1p th\xec parser ph\u1ea3i tr\u1ea3 v\u1ec1 l\u1ed7i t\u01b0\u01a1ng \u1ee9ng. V\xe0 trong th\u1ebf gi\u1edbi c\u1ee7a Functional Programming th\xec ch\xfang ta ph\u1ea3i th\u1ec3 hi\u1ec7n \u0111i\u1ec1u \u0111\xf3 khi khai b\xe1o ki\u1ec3u cho parser. Sau \u0111\xe2y l\xe0 \u0111\u1ecbnh ngh\u0129a m\u1edbi c\u1ee7a parser b\u1eb1ng c\xe1ch s\u1eed d\u1ee5ng Either ",(0,p.kt)("sup",{parentName:"p",id:"fnref-either"},(0,p.kt)("a",{parentName:"sup",href:"#fn-either",className:"footnote-ref"},"either"))),(0,p.kt)("pre",null,(0,p.kt)("code",{parentName:"pre",className:"language-scala"},"type Parser[A] = String => Either[ParseError, A]\n")),(0,p.kt)("p",null,"\u0110i\u1ec1u ti\u1ebfp theo l\xe0 nh\u01b0 ch\xfang ta \u0111\xe3 n\xf3i \u1edf tr\xean v\u1ec1 parser combinator, ch\xfang ta kh\xf4ng mu\u1ed1n vi\u1ebft m\u1ed9t c\xe1i parser ph\u1ee9c t\u1ea1p m\u1ed9t l\u1ea7n duy nh\u1ea5t, m\xe0 ch\xfang ta mu\u1ed1n vi\u1ebft nh\u1eefng parser \u0111\u01a1n gi\u1ea3n, c\u01a1 b\u1ea3n r\u1ed3i k\u1ebft h\u1ee3p ch\xfang l\u1ea1i v\u1edbi nhau. V\xed d\u1ee5 nh\u01b0 khi parse m\u1ed9t bi\u1ec3u th\u1ee9c to\xe1n h\u1ecdc \u0111\u01a1n gi\u1ea3n nh\u01b0 sau ",(0,p.kt)("inlineCode",{parentName:"p"},"(3 + 5) * 4")," ch\xfang ta s\u1ebd vi\u1ebft m\u1ed9t c\xe1i parser cho d\u1ea5u ",(0,p.kt)("inlineCode",{parentName:"p"},"(")," r\u1ed3i m\u1ed9t parser kh\xe1c cho s\u1ed1 t\u1ef1 nhi\xean, r\u1ed3i c\xe1c ph\xe9p to\xe1n ... Do \u0111\xf3 parser c\u1ee7a ch\xfang ta kh\xf4ng n\xean ti\xeau th\u1ee5 ho\xe0n to\xe0n String input m\xe0 ch\u1ec9 n\xean s\u1eed d\u1ee5ng m\u1ed9t ph\u1ea7n v\xe0 tr\u1ea3 v\u1ec1 ph\u1ea7n c\xf2n l\u1ea1i cho c\xe1c parser ti\u1ebfp theo ti\xeau th\u1ee5. T\u1eeb \u0111\xf3 ch\xfang ta c\xf3 m\u1ed9t \u0111\u1ecbnh ngh\u0129a m\xe0 ch\xfang ta c\xf3 th\u1ec3 tho\xe3 m\xe3n nh\u01b0 sau:"),(0,p.kt)("pre",null,(0,p.kt)("code",{parentName:"pre",className:"language-scala"},"type Parser[A] = String => Either[Parser.Error, (String, A)]\n")),(0,p.kt)("p",null,"Trong th\u1ef1c t\u1ebf th\xec Parser \u0111\u01b0\u1ee3c \u0111\u1ecbnh ngh\u0129a ph\u1ee9c t\u1ea1p h\u01a1n m\u1ed9t x\xedu nh\u01b0 sau:"),(0,p.kt)("pre",null,(0,p.kt)("code",{parentName:"pre",className:"language-scala"},"sealed abstract class Parser[+A] {\n  final def parse(str: String): Either[Parser.Error, (String, A)]\n\n  // Attempt to parse all of the input `str` into an `A` value.\n  final def parseAll(str: String): Either[Parser.Error, A]\n}\n\n")),(0,p.kt)("h2",{id:"parser-with-cats"},"Parser with cats"),(0,p.kt)("p",null,"Sau khi \u0111\xe3 c\xf3 \u0111\u1ecbnh ngh\u0129a c\u1ee7a parser, ch\xfang ta s\u1ebd kh\xe1m c\xe1ch s\u1eed d\u1ee5ng parser v\xe0 parser combinator b\u1eb1ng library ",(0,p.kt)("a",{parentName:"p",href:"https://github.com/typelevel/cats-parse"},"cats-parse")," - m\u1ed9t trong nh\u1eefng parser combinators library cho ng\xf4n ng\u1eef Scala. C\xe1c ph\u1ea7n sau \u0111a ph\u1ea7n \u0111\u01b0\u1ee3c l\u1ea5y tr\u1ef1c ti\u1ebfp t\u1eeb ",(0,p.kt)("a",{parentName:"p",href:"https://github.com/typelevel/cats-parse#readme"},"cats-parse")," v\u1edbi m\u1ed9t s\u1ed1 ch\u1ec9nh s\u1eeda."),(0,p.kt)("h3",{id:"simple-parsers"},"Simple Parsers"),(0,p.kt)("p",null,"Library cats-parse cung c\u1ea5p m\u1ed9t t\u1eadp h\u1ee3p c\xe1c parser c\u01a1 b\u1ea3n, \u0111\u1ec3 t\u1ea1o th\xe0nh c\xe1c building block cho b\u1ea5t c\u1ee9 parser ph\u1ee9c t\u1ea1p n\xe0o."),(0,p.kt)("p",null,"\u0110\u1ea7u ti\xean l\xe0 ",(0,p.kt)("inlineCode",{parentName:"p"},"Parser.anyChar"),", l\xe0 m\u1ed9t parser lu\xf4n lu\xf4n tr\u1ea3 v\u1ec1 k\xfd t\u1ef1 \u0111\u1ea7u ti\xean c\u1ee7a chu\u1ed7i input (fail trong tr\u01b0\u1eddng h\u1ee3p \u0111\u1ea7u v\xe0o l\xe0 m\u1ed9t empty string)."),(0,p.kt)("pre",null,(0,p.kt)("code",{parentName:"pre",className:"language-scala"},'val p: Parser[Char] = Parser.anyChar\n\np.parse("t")\n// Either[Error, Tuple2[String, Char]] = Right((,t))\np.parse("")\n// Either[Error, Tuple2[String, Char]] = Left(Error(0,NonEmptyList(InRange(0,,))))\np.parse("two")\n// Either[Error, Tuple2[String, Char]] = Right((wo,t))\n')),(0,p.kt)("p",null,(0,p.kt)("inlineCode",{parentName:"p"},"Parser.string")," l\xe0 parser m\xe0 n\xf3 s\u1ebd parse th\xe0nh c\xf4ng n\u1ebfu string input b\u1eaft \u0111\u1ea7u v\u1edbi gi\xe1 tr\u1ecb c\u1ee7a ",(0,p.kt)("inlineCode",{parentName:"p"},"str"),". Ch\xfa \xfd r\u1eb1ng ",(0,p.kt)("inlineCode",{parentName:"p"},"Parser.string")," s\u1ebd tr\u1ea3 v\u1ec1 m\u1ed9t parser c\xf3 type l\xe0 ",(0,p.kt)("inlineCode",{parentName:"p"},"Parser[Unit]"),", \u0111i\u1ec1u \u0111\xf3 c\xf3 ngh\u0129a l\xe0 n\xf3 s\u1ebd tr\u1ea3 v\u1ec1 ",(0,p.kt)("inlineCode",{parentName:"p"},"Unit")," n\u1ebfu th\xe0nh c\xf4ng."),(0,p.kt)("pre",null,(0,p.kt)("code",{parentName:"pre",className:"language-scala"},'val p: Parser[Unit] = Parser.string("hello")\n\np.parse("hello")\n// Either[Error, Tuple2[String, Unit]] = Right((,()))\np.parse("hell")\n// Either[Error, Tuple2[String, Unit]] = Left(Error(0,NonEmptyList(OneOfStr(0,List(hello)))))\np.parse("hello world")\n// Either[Error, Tuple2[String, Unit]] = Right(( world ,hello))\n')),(0,p.kt)("p",null,(0,p.kt)("inlineCode",{parentName:"p"},"sp")," t\u01b0\u01a1ng t\u1ef1 nh\u01b0 ",(0,p.kt)("inlineCode",{parentName:"p"},"Parser.anyChar")," nh\u01b0ng ch\u1ec9 \u0111\xfang khi k\xfd t\u1ef1 \u0111\u1ea7u ti\xean l\xe0 k\xfd t\u1ef1 kho\u1ea3ng tr\u1eafng."),(0,p.kt)("pre",null,(0,p.kt)("code",{parentName:"pre",className:"language-scala"},'import cats.parse.Rfc5234.sp\n\nsp.parse(" ")\n// Either[Error, Tuple2[String, Unit]] = Right((,()))\nsp.parse("o_o")\n// Either[Error, Tuple2[String, Unit]] = Left(Error(0,NonEmptyList(InRange(0, , ))))\n\n')),(0,p.kt)("p",null,(0,p.kt)("inlineCode",{parentName:"p"},"alpha")," t\u01b0\u01a1ng t\u1ef1 nh\u01b0 ",(0,p.kt)("inlineCode",{parentName:"p"},"Parser.anyChar")," nh\u01b0ng ch\u1ec9 \u0111\xfang khi k\xfd t\u1ef1 \u0111\u1ea7u ti\xean l\xe0 k\xfd t\u1ef1 alphabet."),(0,p.kt)("pre",null,(0,p.kt)("code",{parentName:"pre",className:"language-scala"},'import cats.parse.Rfc5234.alpha\nalpha.parse("z")\n// Either[Error, Tuple2[String, Char]] = Right((,z))\nalpha.parse("3")\n// Either[Error, Tuple2[String, Char]] = Left(Error(0,NonEmptyList(InRange(0,A,Z), InRange(0,a,z))))\n')),(0,p.kt)("p",null,(0,p.kt)("inlineCode",{parentName:"p"},"digit")," t\u01b0\u01a1ng t\u1ef1 nh\u01b0 ",(0,p.kt)("inlineCode",{parentName:"p"},"Parser.alpha")," nh\u01b0ng ch\u1ec9 \u0111\xfang khi k\xfd t\u1ef1 \u0111\u1ea7u ti\xean l\xe0 k\xfd t\u1ef1 t\u1eeb 0-9"),(0,p.kt)("pre",null,(0,p.kt)("code",{parentName:"pre",className:"language-scala"},'import cats.parse.Rfc5234.digit\n\ndigit.parse("3")\n// Either[Error, Tuple2[String, Char]] = Right((,3))\ndigit.parse("z")\n//  Either[Error, Tuple2[String, Char]] = Left(Error(0,NonEmptyList(InRange(0,0,9))))\n')),(0,p.kt)("p",null,(0,p.kt)("inlineCode",{parentName:"p"},"Parser.charIn")," nh\u1eadn m\u1ed9t string \u0111\u1ea7u v\xe0o v\xe0 tr\u1ea3 v\u1ec1 m\u1ed9t parser m\xe0 n\xf3 s\u1ebd parse th\xe0nh c\xf4ng n\u1ebfu k\xfd t\u1ef1 \u0111\u1ea7u ti\xean l\xe0 m\u1ed9t character trong string \u0111\u1ea7u v\xe0o."),(0,p.kt)("pre",null,(0,p.kt)("code",{parentName:"pre",className:"language-scala"},'val charIn = Parser.charIn("123456789") // t\u01b0\u01a1ng \u0111\u01b0\u01a1ng v\u1edbi digit\ncharIn.parse("3")\n// Either[Error, Tuple2[String, Char]] = Right((,3))\n')),(0,p.kt)("h3",{id:"mapping-output"},"Mapping Output"),(0,p.kt)("p",null,"\u0110\u1ea7u ra c\u1ee7a parser c\xf3 th\u1ec3 \u0111\u01b0\u1ee3c x\u1eed l\xfd b\u1eb1ng ",(0,p.kt)("inlineCode",{parentName:"p"},"map")," function."),(0,p.kt)("pre",null,(0,p.kt)("code",{parentName:"pre",className:"language-scala"},'case class CharWrapper(value: Char)\n\nval p: Parser[CharWrapper] = Parser.anyChar.map(char => CharWrapper(char))\n\np.parse("t")\n// Right((,CharWrapper(t)))\n')),(0,p.kt)("p",null,"Library cung c\u1ea5p s\u1eb5n m\u1ed9t s\u1ed1 h\xe0m cho \u0111\u1ec3 mapping sang type ",(0,p.kt)("inlineCode",{parentName:"p"},"String")," v\xe0 ",(0,p.kt)("inlineCode",{parentName:"p"},"Unit")," d\u1ec5 d\xe0ng h\u01a1n"),(0,p.kt)("pre",null,(0,p.kt)("code",{parentName:"pre",className:"language-scala"},'/* String */\n\nval p2: Parser[String] = digit.map((c: Char) => c.toString)\n// t\u01b0\u01a1ng \u0111\u01b0\u01a1ng v\u1edbi\nval p3: Parser[String] = digit.string\n\np3.parse("1")\n// Either[Error, Tuple2[String, String]] = Right((,1))\n\n/* Unit */\n\nval p4: Parser[Unit] = digit.map(_ => ())\n// t\u01b0\u01a1ng \u0111\u01b0\u01a1ng v\u1edbi\nval p5: Parser[Unit] = digit.void\n\np5.parse("1")\n// Either[Error, Tuple2[String, Unit]] = Right((,()))\n')),(0,p.kt)("p",null,(0,p.kt)("em",{parentName:"p"},"Ch\xfa \xfd"),": ",(0,p.kt)("inlineCode",{parentName:"p"},"string")," s\u1ebd tr\u1ea3 v\u1ec1 input m\xe0 parser \u0111\xe3 s\u1eed d\u1ee5ng, v\xe0 b\u1ecf qua output."),(0,p.kt)("h3",{id:"combinging-parsers"},"Combinging parsers"),(0,p.kt)("p",null,"C\xe1c parser c\xf3 th\u1ec3 k\u1ebft h\u1ee3p v\u1edbi nhau b\u1eb1ng c\xe1c operator sau:"),(0,p.kt)("ul",null,(0,p.kt)("li",{parentName:"ul"},(0,p.kt)("inlineCode",{parentName:"li"},"~")," - ",(0,p.kt)("inlineCode",{parentName:"li"},"product")," - Ti\u1ebfp t\u1ee5c parsing parser th\u1ee9 2 n\u1ebfu parser \u0111\u1ea7u ti\xean th\xe0nh c\xf4ng;"),(0,p.kt)("li",{parentName:"ul"},(0,p.kt)("inlineCode",{parentName:"li"},"<*")," - ",(0,p.kt)("inlineCode",{parentName:"li"},"productL")," - Nh\u01b0 h\xe0m ",(0,p.kt)("inlineCode",{parentName:"li"},"product")," nh\u01b0ng b\u1ecf qua k\u1ebft qu\u1ea3 c\u1ee7a parser th\u1ee9 2;"),(0,p.kt)("li",{parentName:"ul"},(0,p.kt)("inlineCode",{parentName:"li"},"*>")," - ",(0,p.kt)("inlineCode",{parentName:"li"},"productR")," - Nh\u01b0 h\xe0m ",(0,p.kt)("inlineCode",{parentName:"li"},"product")," nh\u01b0ng b\u1ecf qua k\u1ebft qu\u1ea3 c\u1ee7a parser \u0111\u1ea7u ti\xean;"),(0,p.kt)("li",{parentName:"ul"},(0,p.kt)("inlineCode",{parentName:"li"},"suroundedBy")," - t\u01b0\u01a1ng \u0111\u01b0\u01a1ng v\u1edbi ",(0,p.kt)("inlineCode",{parentName:"li"},"border *> parsingResult <* border"),";"),(0,p.kt)("li",{parentName:"ul"},(0,p.kt)("inlineCode",{parentName:"li"},"between")," - t\u01b0\u01a1ng \u0111\u01b0\u01a1ng v\u1edbi ",(0,p.kt)("inlineCode",{parentName:"li"},"border1 *> parsingResult <* border2"),";"),(0,p.kt)("li",{parentName:"ul"},(0,p.kt)("inlineCode",{parentName:"li"},"|")," - ",(0,p.kt)("inlineCode",{parentName:"li"},"orElse")," - Parser th\xe0nh c\xf4ng n\u1ebfu m\u1ed9t trong hai parser \u0111\u1ea7u v\xe0o th\xe0nh c\xf4ng;")),(0,p.kt)("pre",null,(0,p.kt)("code",{parentName:"pre",className:"language-scala"},'import cats.parse.Rfc5234.{sp, alpha, digit}\nimport cats.parse.Parser\n\n/* Product */\n\n// product\nval p1: Parser[(Char, Unit)] = alpha ~ sp\n\np1.parse("t")\n// Either[Error, Tuple2[String, Tuple2[Char, Unit]]] = Left(Error(1,NonEmptyList(InRange(1, , ))))\np1.parse("t ")\n// Either[Error, Tuple2[String, Tuple2[Char, Unit]]] = Right((,(t,())))\n\n/* productL, productR */\n\n// T\u01b0\u01a1ng t\u1ef1 nh\u01b0 `p1` nh\u01b0ng \u0111\u1ea7u ra s\u1ebd c\xf3 type l\xe0 `Parser[Char]` thay v\xec `Tuple2`\n// v\xec <* s\u1ebd b\u1ecf qu\u1ea3 k\u1ebft qu\u1ea3 c\u1ee7a parser ph\xeda b\xean tr\xe1i\nval p2: Parser[Char] = alpha <* sp\n\np2.parse("t")\n// Either[Error, Tuple2[String, Char]] = Left(Error(1,NonEmptyList(InRange(1, , ))))\np2.parse("t ")\n// Either[Error, Tuple2[String, Char]] = Right((,t))\n\n// Ch\xfa \xfd n\u1ebfu mu\u1ed1n b\u1ecf qua k\u1ebft qu\u1ea3 c\u1ee7a alpha th\xec chuy\u1ec3n m\u0169i t\xean\nval p21: Parser[Unit] = alpha *> sp\n\n/* surroundedBy */\n\nval p4: Parser[Char] = sp *> alpha <* sp\nval p5: Parser[Char] = alpha.surroundedBy(sp)\n\np4.parse(" a ")\n// Either[Error, Tuple2[String, Char]] = Right((,a))\np5.parse(" a ")\n// Either[Error, Tuple2[String, Char]] = Right((,a))\n\n/* between */\n\nval p6: Parser[Char] = sp *> alpha <* digit\nval p7: Parser[Char] = alpha.between(sp, digit)\n\np6.parse(" a1")\n// Either[Error, Tuple2[String, Char]] = Right((,a))\np7.parse(" a1")\n// Either[Error, Tuple2[String, Char]] = Right((,a))\n\n/* OrElse */\n\nval p3: Parser[AnyVal] = alpha | sp\n\np3.parse("t")\n// Either[Error, Tuple2[String, AnyVal]] = Right((,t))\np3.parse(" ")\n// Either[Error, Tuple2[String, AnyVal]] = Right((,()))\n')),(0,p.kt)("h3",{id:"repeating-parsers"},"Repeating parsers"),(0,p.kt)("p",null,(0,p.kt)("inlineCode",{parentName:"p"},"cats-parse")," cung c\u1ea5p 2 function \u0111\u1ec3 ch\xfang ta bi\u1ebfn m\u1ed9t Parser","[A]"," th\xe0nh Paser[List","[A]","] \u0111\xf3 l\xe0 ",(0,p.kt)("inlineCode",{parentName:"p"},"rep")," v\xe0 ",(0,p.kt)("inlineCode",{parentName:"p"},"rep0"),". V\u1edbi ",(0,p.kt)("inlineCode",{parentName:"p"},"rep")," parser c\u1ea7n ph\u1ea3i parse th\xe0nh c\xf4ng \xedt nh\u1ea5t m\u1ed9t ph\u1ea7n t\u1eed, c\xf2n ",(0,p.kt)("inlineCode",{parentName:"p"},"rep0")," th\xec c\xf3 th\u1ec3 cho ra m\u1ed9t List r\u1ed7ng."),(0,p.kt)("pre",null,(0,p.kt)("code",{parentName:"pre",className:"language-scala"},'val number: Parser[NonEmptyList[Char]] = digit.rep\nval numberOrNone: Parser0[List[Char]] = digit.rep0\n\nnumber.parse("73")\n// Either[Error, Tuple2[String, NonEmptyList[Char]]] = Right((,NonEmptyList(7, 3)))\nnumber.parse("")\n// Either[Error, Tuple2[String, NonEmptyList[Char]]] = Left(Error(0,NonEmptyList(InRange(0,0,9))))\nnumberOrNone.parse("")\n// Either[Error, Tuple2[String, List[Char]]] = Right((,List()))\nnumberOrNone.parse("73")\n// Either[Error, Tuple2[String, List[Char]]] = Right((,List(7, 3)))\n')),(0,p.kt)("p",null,(0,p.kt)("strong",{parentName:"p"},"Ch\xfa \xfd"),": type c\u1ee7a number v\xe0 numberOrNone l\xe0 kh\xe1c nhau. ",(0,p.kt)("inlineCode",{parentName:"p"},"Parser")," type lu\xf4n lu\xf4n tr\u1ea3 v\u1ec1 non-empty ouput, c\xf2n ",(0,p.kt)("inlineCode",{parentName:"p"},"Parser0")," th\xec c\xf3 th\u1ec3 empty(trong tr\u01b0\u1eddng h\u1ee3p th\xe0nh c\xf4ng)."),(0,p.kt)("p",null,(0,p.kt)("inlineCode",{parentName:"p"},"rep")," v\xe0 ",(0,p.kt)("inlineCode",{parentName:"p"},"rep0")," c\xf3 th\u1ec3 k\u1ebft h\u1ee3p v\u1edbi ",(0,p.kt)("inlineCode",{parentName:"p"},"string")," function m\xe0 ch\xfang ta \u0111\xe3 nh\u1eafc \u0111\u1ebfn \u1edf tr\xean."),(0,p.kt)("pre",null,(0,p.kt)("code",{parentName:"pre",className:"language-scala"},'val word1 = alpha.rep.map((l: NonEmptyList[Char]) => l.toList.mkString)\nval word2 = alpha.rep.string\nval word2 = alpha.repAs[String]\n\nword1.parse("bla")\n// Either[Error, Tuple2[String, String]] = Right((,bla))\n')),(0,p.kt)("p",null,"3 parser \u1edf tr\xean ho\xe0n to\xe0n gi\u1ed1ng nhau v\u1ec1 m\u1eb7t k\u1ebft qu\u1ea3, nh\u01b0ng 2 parser sau s\u1ebd t\u1ed1i \u01b0u h\u01a1n v\xec ch\xfang kh\xf4ng ph\u1ea3i t\u1ea1o ra List trung gian"),(0,p.kt)("h4",{id:"parsers-with-empty-output"},"Parsers with empty output"),(0,p.kt)("p",null,"C\xf3 m\u1ed9t s\u1ed1 parser kh\xf4ng bao gi\u1edd tr\u1ea3 v\u1ec1 k\u1ebft qu\u1ea3 v\xe0 type c\u1ee7a ch\xfang s\u1ebd l\xe0 ",(0,p.kt)("inlineCode",{parentName:"p"},"Parser0"),". Ch\xfang ta c\xf3 th\u1ec3 chuy\u1ec3n type ",(0,p.kt)("inlineCode",{parentName:"p"},"Parser")," v\u1ec1 ",(0,p.kt)("inlineCode",{parentName:"p"},"Parser0")," b\u1eb1ng ",(0,p.kt)("inlineCode",{parentName:"p"},"rep0")," ho\u1eb7c ",(0,p.kt)("inlineCode",{parentName:"p"},"?")," aka ",(0,p.kt)("inlineCode",{parentName:"p"},"optional"),"."),(0,p.kt)("pre",null,(0,p.kt)("code",{parentName:"pre",className:"language-scala"},'val p: Parser[String] = (alpha.rep <* sp.?).rep.string\n\np.parse("hello world")\n// Either[Error, Tuple2[String, String]] = Right((,hello world))\n')),(0,p.kt)("h3",{id:"error-handling"},"Error Handling"),(0,p.kt)("p",null,"Nh\u01b0 ch\xfang ta \u0111\xe3 n\xf3i \u1edf ph\u1ea7n \u0111\u1ea7u ti\xean, m\u1ed9t parser n\u1ebfu parse kh\xf4ng th\xe0nh c\xf4ng th\xec s\u1ebd ph\u1ea3i tr\u1ea3 v\u1ec1 l\u1ed7i. C\xf3 2 ki\u1ec3u l\u1ed7i:"),(0,p.kt)("ul",null,(0,p.kt)("li",{parentName:"ul"},(0,p.kt)("em",{parentName:"li"},"epsilon failure"),": l\u1ed7i m\xe0 parser ch\u01b0a s\u1eed d\u1ee5ng b\u1ea5t k\u1ef3 m\u1ed9t character n\xe0o"),(0,p.kt)("li",{parentName:"ul"},(0,p.kt)("em",{parentName:"li"},"arresting failure"),": l\u1ed7i m\xe0 parser \u0111\xe3 s\u1eed d\u1ee5ng \xedt nh\u1ea5t m\u1ed9t character")),(0,p.kt)("p",null,"V\u1ec1 m\u1eb7t implementation ",(0,p.kt)("inlineCode",{parentName:"p"},"Parser.Error")," c\xf3 \u0111\u1ecbnh ngh\u0129a nh\u01b0 sau:"),(0,p.kt)("pre",null,(0,p.kt)("code",{parentName:"pre",className:"language-scala"},"final case class Error(failedAtOffset: Int, expected: NonEmptyList[Expectation])\n")),(0,p.kt)("p",null,"N\u1ebfu ",(0,p.kt)("inlineCode",{parentName:"p"},"failedAtOffset == 0")," th\xec \u0111\xf3 l\xe0 ",(0,p.kt)("inlineCode",{parentName:"p"},"epsilon failure")," v\xe0 ",(0,p.kt)("inlineCode",{parentName:"p"},"arresting failure")," trong tr\u01b0\u1eddng h\u1ee3p c\xf2n l\u1ea1i."),(0,p.kt)("pre",null,(0,p.kt)("code",{parentName:"pre",className:"language-scala"},'val p1: Parser[Char] = alpha\nval p2: Parser[Char] = sp *> alpha\n\n// epsilon failure\np1.parse("123")\n// Either[Error, Tuple2[String, Char]] = Left(Error(0,NonEmptyList(InRange(0,A,Z), InRange(0,a,z))))\n\n// arresting failure\np2.parse(" 1")\n// Either[Error, Tuple2[String, Char]] = Left(Error(1,NonEmptyList(InRange(1,A,Z), InRange(1,a,z))))\n')),(0,p.kt)("p",null,"Ch\xfang ta c\u1ea7n ph\xe2n bi\u1ec7t hai lo\u1ea1i l\u1ed7i n\xe0y v\xec, lo\u1ea1i \u0111\u1ea7u ti\xean cho ch\xfang ta bi\u1ebft parser kh\xf4ng kh\u1edbp v\u1edbi input \u0111\u1ea7u v\xe0o ngay khi b\u1eaft \u0111\u1ea7u parse, v\xe0 lo\u1ea1i th\u1ee9 2 x\u1ea3y ra trong qu\xe1 tr\xecnh parse."),(0,p.kt)("h3",{id:"backtrack"},"Backtrack"),(0,p.kt)("p",null,"Backtrack l\xe0 m\u1ed9t function gi\xfap ch\xfang ta chuy\u1ec3n ",(0,p.kt)("em",{parentName:"p"},"arresting failure")," th\xe0nh ",(0,p.kt)("em",{parentName:"p"},"epsilon failure"),". N\xf3 c\u0169ng gi\xfap tua l\u1ea1i offset c\u1ee7a input v\u1ec1 tr\u01b0\u1edbc khi parser b\u1eaft \u0111\u1ea7u. \u0110\xe2y l\xe0 m\u1ed9t function c\u1ef1c k\u1ef3 h\u1eefu d\u1ee5ng khi ch\xfang ta mu\u1ed1n k\u1ebft h\u1ee3p nhi\u1ec1u parser l\u1ea1i v\u1edbi nhau."),(0,p.kt)("pre",null,(0,p.kt)("code",{parentName:"pre",className:"language-scala"},'val p1 = sp *> digit <* sp // _digit_\nval p2 = sp *> digit // _digit\n\np1.parse(" 1") // (1)\n// Left(Error(2,NonEmptyList(InRange(2, , ))))\n\n(p1 | p2 ).parse(" 1") // (2)\n// Either[Error, Tuple2[String, Char]] = Left(Error(2,NonEmptyList(InRange(2, , ))))\n\n(p1.backtrack | p2 ).parse(" 1") (3)\n// Either[Error, Tuple2[String, Char]] = Right((,1))\n')),(0,p.kt)("p",null,"Parser(2) tr\u1ea3 v\u1ec1 l\u1ed7i b\u1edfi v\xec p1 tr\u1ea3 v\u1ec1 ",(0,p.kt)("em",{parentName:"p"},"arresting failure")," v\xe0 operator ",(0,p.kt)("inlineCode",{parentName:"p"},"|")," ch\u1ec9 c\xf3 th\u1ec3 ph\u1ee5c h\u1ed3i v\u1edbi ",(0,p.kt)("em",{parentName:"p"},"epsilon failure"),"."),(0,p.kt)("p",null,"Parser(3) parse th\xe0nh c\xf4ng v\xec ",(0,p.kt)("inlineCode",{parentName:"p"},"backtrack")," chuy\u1ec3n ",(0,p.kt)("em",{parentName:"p"},"arresting failure")," th\xe0nh ",(0,p.kt)("em",{parentName:"p"},"epsilon failure")," n\xean sau khi th\u1ea5t b\u1ea1i v\u1edbi ",(0,p.kt)("inlineCode",{parentName:"p"},"p1.backtrack")," (3) s\u1ebd ti\u1ebfp t\u1ee5c v\u1edbi ",(0,p.kt)("inlineCode",{parentName:"p"},"p2")," v\xe0 th\xe0nh c\xf4ng."),(0,p.kt)("h3",{id:"soft"},"Soft"),(0,p.kt)("p",null,"C\xf3 hi\u1ec7u \u1ee9ng t\u01b0\u01a1ng t\u1ef1 nh\u01b0 ",(0,p.kt)("inlineCode",{parentName:"p"},"backtrack")," v\u1edbi ",(0,p.kt)("inlineCode",{parentName:"p"},"|"),", nh\u01b0ng v\u1edbi operator ",(0,p.kt)("inlineCode",{parentName:"p"},"~")," v\xe0 n\xf3 cho ph\xe9p ch\xfang ta ti\u1ebfp t\u1ee5c parsing khi to\xe1n t\u1eed b\xean ph\u1ea3i tr\u1ea3 v\u1ec1 ",(0,p.kt)("inlineCode",{parentName:"p"},"epsilon failure"),". N\xf3 r\u1ea5t h\u1eefu \xedch trong tr\u01b0\u1eddng h\u1ee3p ch\xfang ta kh\xf4ng bi\u1ebft ch\xednh x\xe1c output m\xe0 ch\xfang ta c\u1ea7n tr\u01b0\u1edbc khi qu\xe1 tr\xecnh parsing k\u1ebft th\xfac. Nh\u01b0 v\xed d\u1ee5 d\u01b0\u1edbi \u0111\xe2y ch\xfang ta parse \u0111\u1ea7u v\xe0o cho m\u1ed9t search engine. Input c\xf3 th\u1ec3 c\xf3 d\u1ea1ng ",(0,p.kt)("inlineCode",{parentName:"p"},"key:value")," ho\u1eb7c ch\u1ec9 m\u1ed7i ",(0,p.kt)("inlineCode",{parentName:"p"},"value")),(0,p.kt)("pre",null,(0,p.kt)("code",{parentName:"pre",className:"language-scala"},'val searchWord = (alpha.rep.string ~ sp.?).rep.string\nval fieldValue = alpha.rep.string ~ pchar(\':\')\n\nval fieldValueSoft = alpha.rep.string.soft ~ pchar(\':\')\n\nval p1 = fieldValue.? ~ searchWord\nval p2 = fieldValueSoft.? ~ searchWord\n\np2.parse("title:The Wind Has Risen") (1)\n// Right((,(Some((title,())),The Wind Has Risen)))\np2.parse("The Wind Has Risen") // (2)\n// Right((,(None,The Wind Has Risen)))\np1.parse("The Wind Has Risen") // (3)\n// Left(Error(3,NonEmptyList(InRange(3,:,:))))\n')),(0,p.kt)("p",null,"Parser ",(0,p.kt)("inlineCode",{parentName:"p"},"p2")," parse th\xe0nh c\xf4ng \u1edf (2) trong khi ",(0,p.kt)("inlineCode",{parentName:"p"},"p1")," th\u1ea5t b\u1ea1i \u1edf (3) b\u1edfi v\xec ",(0,p.kt)("inlineCode",{parentName:"p"},"fieldValueSoft")," tr\u1ea3 v\u1ec1 ",(0,p.kt)("em",{parentName:"p"},"epsilon failure")," v\u1edbi s\u1ef1 gi\xfap \u0111\u1ee1 c\u1ee7a ",(0,p.kt)("inlineCode",{parentName:"p"},"soft")," t\u1eeb \u0111\xf3 ",(0,p.kt)("inlineCode",{parentName:"p"},"p2")," c\xf3 th\u1ec3 ti\u1ebfp t\u1ee5c parsing v\u1edbi ",(0,p.kt)("inlineCode",{parentName:"p"},"searchWord")," trong khi ",(0,p.kt)("inlineCode",{parentName:"p"},"p1")," tr\u1ea3 v\u1ec1 l\u1ed7i v\xec n\xf3 nh\u1eadn \u0111\u01b0\u1ee3c ",(0,p.kt)("em",{parentName:"p"},"arresting failure")," t\u1eeb ",(0,p.kt)("inlineCode",{parentName:"p"},"failValue")," parser."),(0,p.kt)("h2",{id:"resources"},"Resources"),(0,p.kt)("ul",null,(0,p.kt)("li",{parentName:"ul"},(0,p.kt)("a",{parentName:"li",href:"https://hasura.io/blog/parser-combinators-walkthrough"},"Parser Combinators Walkthrough")),(0,p.kt)("li",{parentName:"ul"},(0,p.kt)("a",{parentName:"li",href:"https://github.com/typelevel/cats-parse"},"cats-parse")),(0,p.kt)("li",{parentName:"ul"},(0,p.kt)("a",{parentName:"li",href:"https://tgdwyer.github.io/parsercombinators/"},"Parser Combinators tutorial in Haskell"))),(0,p.kt)("div",{className:"footnotes"},(0,p.kt)("hr",{parentName:"div"}),(0,p.kt)("ol",{parentName:"div"},(0,p.kt)("li",{parentName:"ol",id:"fn-1"},(0,p.kt)("p",{parentName:"li"},"Trong ph\u1ea7n \u0111\u1ea7u m\xecnh s\u1ebd s\u1eed d\u1ee5ng pseudocode, implmentation trong th\u1ef1c t\u1ebf s\u1ebd t\u01b0\u01a1ng t\u1ef1 nh\u01b0ng ph\u1ee9c t\u1ea1p h\u01a1n.",(0,p.kt)("a",{parentName:"p",href:"#fnref-1",className:"footnote-backref"},"\u21a9"))),(0,p.kt)("li",{parentName:"ol",id:"fn-either"},(0,p.kt)("p",{parentName:"li"},(0,p.kt)("inlineCode",{parentName:"p"},"Either[E, A]")," \u0111\u01b0\u1ee3c \u0111\u1ecbnh ng\u0129a nh\u01b0 sau(phi\xean b\u1ea3n s\u01a1 l\u01b0\u1ee3c):"),(0,p.kt)("pre",{parentName:"li"},(0,p.kt)("code",{parentName:"pre",className:"language-scala"},"  enum Either[E, A] {\n    case Left(value: E)\n    case Right(value: A)\n  }\n")),(0,p.kt)("p",{parentName:"li"},"l\xe0 ki\u1ec3u d\u1eef li\u1ec7u m\xe0 n\xf3 \u0111\u1ea1i di\u1ec7n cho 2 kh\u1ea3 n\u0103ng ho\u1eb7c l\xe0 Left v\u1edbi gi\xe1 tr\u1ecb c\u1ee7a type E ho\u1eb7c l\xe0 Right v\u1edbi gi\xe1 tr\u1ecb c\u1ee7a type A. Left th\u01b0\u1eddng \u0111\u01b0\u1ee3c d\xf9ng \u0111\u1ec3 bi\u1ec3u th\u1ecb tr\u01b0\u1eddng h\u1ee3p l\u1ed7i, v\xe0 Right cho tr\u01b0\u1eddng h\u1ee3p th\xe0nh c\xf4ng.",(0,p.kt)("a",{parentName:"p",href:"#fnref-either",className:"footnote-backref"},"\u21a9"))))))}g.isMDXComponent=!0}}]);