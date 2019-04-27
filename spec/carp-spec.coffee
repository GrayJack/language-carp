# describe "Janet grammar", ->
  grammar = null

  beforeEach ->
    waitsForPromise ->
      atom.packages.activatePackage("language-carp")

    runs ->
      grammar = atom.grammars.grammarForScopeName("source.carp")

  it "parses the grammar", ->
    expect(grammar).toBeDefined()
    expect(grammar.scopeName).toBe "source.carp"

  it "tokenizes comments", ->
    {tokens} = grammar.tokenizeLine "# carp"
    expect(tokens[0]).toEqual value: "#", scopes: ["source.carp", "comment.line.semicolon.carp", "punctuation.definition.comment.carp"]
    # expect(tokens[1]).toEqual value: " carp", scopes: ["source.carp", "comment.line.semicolon.carp"]

  it "tokenizes shebang comments", ->
    {tokens} = grammar.tokenizeLine "#!/usr/bin/env carp"
    expect(tokens[0]).toEqual value: "#!", scopes: ["source.carp", "comment.line.shebang.carp", "punctuation.definition.comment.shebang.carp"]
    expect(tokens[1]).toEqual value: "/usr/bin/env carp", scopes: ["source.carp", "comment.line.shebang.carp"]

  it "tokenizes strings", ->
    {tokens} = grammar.tokenizeLine '"foo bar"'
    expect(tokens[0]).toEqual value: '"', scopes: ["source.carp", "string.quoted.double.carp", "punctuation.definition.string.begin.carp"]
    expect(tokens[1]).toEqual value: 'foo bar', scopes: ["source.carp", "string.quoted.double.carp"]
    expect(tokens[2]).toEqual value: '"', scopes: ["source.carp", "string.quoted.double.carp", "punctuation.definition.string.end.carp"]

  it "tokenizes character escape sequences", ->
    {tokens} = grammar.tokenizeLine '"\\n"'
    expect(tokens[0]).toEqual value: '"', scopes: ["source.carp", "string.quoted.double.carp", "punctuation.definition.string.begin.carp"]
    expect(tokens[1]).toEqual value: '\\n', scopes: ["source.carp", "string.quoted.double.carp", "constant.character.escape.carp"]
    expect(tokens[2]).toEqual value: '"', scopes: ["source.carp", "string.quoted.double.carp", "punctuation.definition.string.end.carp"]

  it "tokenizes regexes", ->
    {tokens} = grammar.tokenizeLine '"foo"'
    expect(tokens[0]).toEqual value: '"', scopes: ["source.carp", "string.regexp.carp", "punctuation.definition.regexp.begin.carp"]
    expect(tokens[1]).toEqual value: 'foo', scopes: ["source.carp", "string.regexp.carp"]
    expect(tokens[2]).toEqual value: '"', scopes: ["source.carp", "string.regexp.carp", "punctuation.definition.regexp.end.carp"]

  # it "tokenizes backslash escape character in regexes", ->
  #   {tokens} = grammar.tokenizeLine '"\\\\" "/"'
  #   expect(tokens[0]).toEqual value: '#"', scopes: ["source.carp", "string.regexp.carp", "punctuation.definition.regexp.begin.carp"]
  #   expect(tokens[1]).toEqual value: "\\\\", scopes: ['source.carp', 'string.regexp.carp', 'constant.character.escape.carp']
  #   expect(tokens[2]).toEqual value: '"', scopes: ['source.carp', 'string.regexp.carp', "punctuation.definition.regexp.end.carp"]
  #   expect(tokens[4]).toEqual value: '"', scopes: ['source.carp', 'string.quoted.double.carp', 'punctuation.definition.string.begin.carp']
  #   expect(tokens[5]).toEqual value: "/", scopes: ['source.carp', 'string.quoted.double.carp']
  #   expect(tokens[6]).toEqual value: '"', scopes: ['source.carp', 'string.quoted.double.carp', 'punctuation.definition.string.end.carp']
  #
  # it "tokenizes escaped double quote in regexes", ->
  #   {tokens} = grammar.tokenizeLine '#"\\""'
  #   expect(tokens[0]).toEqual value: '#"', scopes: ["source.carp", "string.regexp.carp", "punctuation.definition.regexp.begin.carp"]
  #   expect(tokens[1]).toEqual value: '\\"', scopes: ['source.carp', 'string.regexp.carp', 'constant.character.escape.carp']
  #   expect(tokens[2]).toEqual value: '"', scopes: ['source.carp', 'string.regexp.carp', "punctuation.definition.regexp.end.carp"]

  it "tokenizes numerics", ->
    numbers =
      # "constant.numeric.ratio.carp": ["1/2", "123/456"]
      "constant.numeric.arbitrary-radix.carp": ["2R1011", "16rDEADBEEF"]
      "constant.numeric.hexadecimal.carp": ["0xDEADBEEF", "0XDEADBEEF"]
      # "constant.numeric.octal.carp": ["0123"]
      "constant.numeric.bigdecimal.carp": ["123.456M"]
      "constant.numeric.double.carp": ["123.45", "123.45e6", "123.45E6"]
      "constant.numeric.bigint.carp": ["123N"]
      "constant.numeric.long.carp": ["123", "12321"]

    for scope, nums of numbers
      for num in nums
        {tokens} = grammar.tokenizeLine num
        expect(tokens[0]).toEqual value: num, scopes: ["source.carp", scope]

  it "tokenizes booleans", ->
    booleans =
      "constant.language.boolean.carp": ["true", "false"]

    for scope, bools of booleans
      for bool in bools
        {tokens} = grammar.tokenizeLine bool
        expect(tokens[0]).toEqual value: bool, scopes: ["source.carp", scope]

  it "tokenizes nil", ->
    {tokens} = grammar.tokenizeLine "nil"
    expect(tokens[0]).toEqual value: "nil", scopes: ["source.carp", "constant.language.nil.carp"]

  it "tokenizes keywords", ->
    tests =
      "meta.expression.carp": ["(:foo)"]
      "meta.map.carp": ["{:foo}"]
      "meta.vector.carp": ["[:foo]"]
      "meta.quoted-expression.carp": ["'(:foo)", "`(:foo)"]

    for metaScope, lines of tests
      for line in lines
        {tokens} = grammar.tokenizeLine line
        expect(tokens[1]).toEqual value: ":foo", scopes: ["source.carp", metaScope, "constant.keyword.carp"]

    {tokens} = grammar.tokenizeLine "(def foo :bar)"
    expect(tokens[5]).toEqual value: ":bar", scopes: ["source.carp", "meta.expression.carp", "meta.definition.global.carp", "constant.keyword.carp"]

  it "tokenizes keyfns (keyword control)", ->
    keyfns = ["import", "require", "def", "def-", "defglobal", "var", "varglobal", "defn", "defn-", "defmacro", "defmacro-"]

    for keyfn in keyfns
      {tokens} = grammar.tokenizeLine "(#{keyfn})"
      expect(tokens[1]).toEqual value: keyfn, scopes: ["source.carp", "meta.expression.carp", "keyword.control.carp"]

  it "tokenizes keyfns (storage control)", ->
    keyfns = ["if", "when", "unless", "for", "cond", "do", "let", "set", "binding", "loop", "fn", "throw", "try", "catch", "for", "while", "break"]

    for keyfn in keyfns
      {tokens} = grammar.tokenizeLine "(#{keyfn})"
      expect(tokens[1]).toEqual value: keyfn, scopes: ["source.carp", "meta.expression.carp", "storage.control.carp"]

  it "tokenizes global definitions", ->
    macros = ["def", "defn", "defn-", "var", "do", "quote", "if", "splice", "while", "set", "quasiquote", "unquote", "break"]

    for macro in macros
      {tokens} = grammar.tokenizeLine "(#{macro} foo 'bar)"
      expect(tokens[1]).toEqual value: macro, scopes: ["source.carp", "meta.expression.carp", "meta.definition.global.carp", "keyword.control.carp"]
      expect(tokens[3]).toEqual value: "foo", scopes: ["source.carp", "meta.expression.carp", "meta.definition.global.carp", "entity.global.carp"]

  it "tokenizes dynamic variables", ->
    mutables = ["@ns", "@foo-bar"]

    for mutable in mutables
      {tokens} = grammar.tokenizeLine mutable
      expect(tokens[0]).toEqual value: mutable, scopes: ["source.carp", "meta.symbol.dynamic.carp"]

  it "tokenizes metadata", ->
    {tokens} = grammar.tokenizeLine "^Foo"
    expect(tokens[0]).toEqual value: "^", scopes: ["source.carp", "meta.metadata.simple.carp"]
    expect(tokens[1]).toEqual value: "Foo", scopes: ["source.carp", "meta.metadata.simple.carp", "meta.symbol.carp"]

    {tokens} = grammar.tokenizeLine "^{:foo true}"
    expect(tokens[0]).toEqual value: "^{", scopes: ["source.carp", "meta.metadata.map.carp", "punctuation.section.metadata.map.begin.carp"]
    expect(tokens[1]).toEqual value: ":foo", scopes: ["source.carp", "meta.metadata.map.carp", "constant.keyword.carp"]
    expect(tokens[2]).toEqual value: " ", scopes: ["source.carp", "meta.metadata.map.carp"]
    expect(tokens[3]).toEqual value: "true", scopes: ["source.carp", "meta.metadata.map.carp", "constant.language.boolean.carp"]
    expect(tokens[4]).toEqual value: "}", scopes: ["source.carp", "meta.metadata.map.carp", "punctuation.section.metadata.map.end.trailing.carp"]

  it "tokenizes functions", ->
    expressions = ["(foo)", "(foo 1 10)"]

    for expr in expressions
      {tokens} = grammar.tokenizeLine expr
      expect(tokens[1]).toEqual value: "foo", scopes: ["source.carp", "meta.expression.carp", "entity.name.function.carp"]

  it "tokenizes vars", ->
    {tokens} = grammar.tokenizeLine "(func #'foo)"
    expect(tokens[2]).toEqual value: " #", scopes: ["source.carp", "meta.expression.carp"]
    expect(tokens[3]).toEqual value: "'foo", scopes: ["source.carp", "meta.expression.carp", "meta.var.carp"]

  it "tokenizes symbols", ->
    {tokens} = grammar.tokenizeLine "foo/bar"
    expect(tokens[0]).toEqual value: "foo", scopes: ["source.carp", "meta.symbol.namespace.carp"]
    expect(tokens[1]).toEqual value: "/", scopes: ["source.carp"]
    expect(tokens[2]).toEqual value: "bar", scopes: ["source.carp", "meta.symbol.carp"]

    {tokens} = grammar.tokenizeLine "x"
    expect(tokens[0]).toEqual value: "x", scopes: ["source.carp", "meta.symbol.carp"]

    # Should not be tokenized as a symbol
    {tokens} = grammar.tokenizeLine "1foobar"
    expect(tokens[0]).toEqual value: "1", scopes: ["source.carp", "constant.numeric.long.carp"]

  testMetaSection = (metaScope, puncScope, startsWith, endsWith) ->
    # Entire expression on one line.
    {tokens} = grammar.tokenizeLine "#{startsWith}foo, bar#{endsWith}"

    [start, mid..., end] = tokens

    expect(start).toEqual value: startsWith, scopes: ["source.carp", "meta.#{metaScope}.carp", "punctuation.section.#{puncScope}.begin.carp"]
    expect(end).toEqual value: endsWith, scopes: ["source.carp", "meta.#{metaScope}.carp", "punctuation.section.#{puncScope}.end.trailing.carp"]

    for token in mid
      expect(token.scopes.slice(0, 2)).toEqual ["source.carp", "meta.#{metaScope}.carp"]

    # Expression broken over multiple lines.
    tokens = grammar.tokenizeLines("#{startsWith}foo\n bar#{endsWith}")

    [start, mid..., after] = tokens[0]

    expect(start).toEqual value: startsWith, scopes: ["source.carp", "meta.#{metaScope}.carp", "punctuation.section.#{puncScope}.begin.carp"]

    for token in mid
      expect(token.scopes.slice(0, 2)).toEqual ["source.carp", "meta.#{metaScope}.carp"]

    [mid..., end] = tokens[1]

    expect(end).toEqual value: endsWith, scopes: ["source.carp", "meta.#{metaScope}.carp", "punctuation.section.#{puncScope}.end.trailing.carp"]

    for token in mid
      expect(token.scopes.slice(0, 2)).toEqual ["source.carp", "meta.#{metaScope}.carp"]

  it "tokenizes expressions", ->
    testMetaSection "expression", "expression", "(", ")"

  it "tokenizes quoted expressions", ->
    testMetaSection "quoted-expression", "expression", "'(", ")"
    testMetaSection "quoted-expression", "expression", "`(", ")"

  it "tokenizes arrays", ->
    testMetaSection "arrays", "arrays", "@[", "]"

  it "tokenizes vectors", ->
    testMetaSection "vector", "vector", "[", "]"

  it "tokenizes tables", ->
    testMetaSection "table", "table", "@{", "}"

  it "tokenizes structs", ->
    testMetaSection "struct", "struct", "{", "}"

  it "tokenizes buffer", ->
    testMetaSection "buffer", "buffer", "@\"", "\""

  # it "tokenizes sets", ->
  #   testMetaSection "set", "set", "\#{", "}"

  it "tokenizes functions in nested sexp", ->
    {tokens} = grammar.tokenizeLine "((foo bar) baz)"
    expect(tokens[0]).toEqual value: "(", scopes: ["source.carp", "meta.expression.carp", "punctuation.section.expression.begin.carp"]
    expect(tokens[1]).toEqual value: "(", scopes: ["source.carp", "meta.expression.carp", "meta.expression.carp", "punctuation.section.expression.begin.carp"]
    expect(tokens[2]).toEqual value: "foo", scopes: ["source.carp", "meta.expression.carp", "meta.expression.carp", "entity.name.function.carp"]
    expect(tokens[3]).toEqual value: " ", scopes: ["source.carp", "meta.expression.carp", "meta.expression.carp"]
    expect(tokens[4]).toEqual value: "bar", scopes: ["source.carp", "meta.expression.carp", "meta.expression.carp", "meta.symbol.carp"]
    expect(tokens[5]).toEqual value: ")", scopes: ["source.carp", "meta.expression.carp", "meta.expression.carp", "punctuation.section.expression.end.carp"]
    expect(tokens[6]).toEqual value: " ", scopes: ["source.carp", "meta.expression.carp"]
    expect(tokens[7]).toEqual value: "baz", scopes: ["source.carp", "meta.expression.carp", "meta.symbol.carp"]
    expect(tokens[8]).toEqual value: ")", scopes: ["source.carp", "meta.expression.carp", "punctuation.section.expression.end.trailing.carp"]

  it "tokenizes maps used as functions", ->
    {tokens} = grammar.tokenizeLine "({:foo bar} :foo)"
    expect(tokens[0]).toEqual value: "(", scopes: ["source.carp", "meta.expression.carp", "punctuation.section.expression.begin.carp"]
    expect(tokens[1]).toEqual value: "{", scopes: ["source.carp", "meta.expression.carp", "meta.map.carp", "punctuation.section.map.begin.carp"]
    expect(tokens[2]).toEqual value: ":foo", scopes: ["source.carp", "meta.expression.carp", "meta.map.carp", "constant.keyword.carp"]
    expect(tokens[3]).toEqual value: " ", scopes: ["source.carp", "meta.expression.carp", "meta.map.carp"]
    expect(tokens[4]).toEqual value: "bar", scopes: ["source.carp", "meta.expression.carp", "meta.map.carp", "meta.symbol.carp"]
    expect(tokens[5]).toEqual value: "}", scopes: ["source.carp", "meta.expression.carp", "meta.map.carp", "punctuation.section.map.end.carp"]
    expect(tokens[6]).toEqual value: " ", scopes: ["source.carp", "meta.expression.carp"]
    expect(tokens[7]).toEqual value: ":foo", scopes: ["source.carp", "meta.expression.carp", "constant.keyword.carp"]
    expect(tokens[8]).toEqual value: ")", scopes: ["source.carp", "meta.expression.carp", "punctuation.section.expression.end.trailing.carp"]

  it "tokenizes sets used in functions", ->
    {tokens} = grammar.tokenizeLine "(\#{:foo :bar})"
    expect(tokens[0]).toEqual value: "(", scopes: ["source.carp", "meta.expression.carp", "punctuation.section.expression.begin.carp"]
    expect(tokens[1]).toEqual value: "\#{", scopes: ["source.carp", "meta.expression.carp", "meta.set.carp", "punctuation.section.set.begin.carp"]
    expect(tokens[2]).toEqual value: ":foo", scopes: ["source.carp", "meta.expression.carp", "meta.set.carp", "constant.keyword.carp"]
    expect(tokens[3]).toEqual value: " ", scopes: ["source.carp", "meta.expression.carp", "meta.set.carp"]
    expect(tokens[4]).toEqual value: ":bar", scopes: ["source.carp", "meta.expression.carp", "meta.set.carp", "constant.keyword.carp"]
    expect(tokens[5]).toEqual value: "}", scopes: ["source.carp", "meta.expression.carp", "meta.set.carp", "punctuation.section.set.end.trailing.carp"]
    expect(tokens[6]).toEqual value: ")", scopes: ["source.carp", "meta.expression.carp", "punctuation.section.expression.end.trailing.carp"]

  describe "firstLineMatch", ->
    it "recognises interpreter directives", ->
      valid = """
        #!/usr/sbin/boot foo
        #!/usr/bin/boot foo=bar/
        #!/usr/sbin/boot
        #!/usr/sbin/boot foo bar baz
        #!/usr/bin/boot perl
        #!/usr/bin/boot bin/perl
        #!/usr/bin/boot
        #!/bin/boot
        #!/usr/bin/boot --script=usr/bin
        #! /usr/bin/env A=003 B=149 C=150 D=xzd E=base64 F=tar G=gz H=head I=tail boot
        #!\t/usr/bin/env --foo=bar boot --quu=quux
        #! /usr/bin/boot
        #!/usr/bin/env boot
      """
      for line in valid.split /\n/
        expect(grammar.firstLineRegex.scanner.findNextMatchSync(line)).not.toBeNull()

      invalid = """
        \x20#!/usr/sbin/boot
        \t#!/usr/sbin/boot
        #!/usr/bin/env-boot/node-env/
        #!/usr/bin/das-boot
        #! /usr/binboot
        #!\t/usr/bin/env --boot=bar
      """
      for line in invalid.split /\n/
        expect(grammar.firstLineRegex.scanner.findNextMatchSync(line)).toBeNull()

    it "recognises Emacs modelines", ->
      valid = """
        #-*- Janet -*-
        #-*- mode: JanetScript -*-
        /* -*-carpScript-*- */
        // -*- Janet -*-
        /* -*- mode:Janet -*- */
        // -*- font:bar;mode:Janet -*-
        // -*- font:bar;mode:Janet;foo:bar; -*-
        // -*-font:mode;mode:Janet-*-
        // -*- foo:bar mode: carpSCRIPT bar:baz -*-
        " -*-foo:bar;mode:carp;bar:foo-*- ";
        " -*-font-mode:foo;mode:carp;foo-bar:quux-*-"
        "-*-font:x;foo:bar; mode : carp; bar:foo;foooooo:baaaaar;fo:ba;-*-";
        "-*- font:x;foo : bar ; mode : JanetScript ; bar : foo ; foooooo:baaaaar;fo:ba-*-";
      """
      for line in valid.split /\n/
        expect(grammar.firstLineRegex.scanner.findNextMatchSync(line)).not.toBeNull()

      invalid = """
        /* --*carp-*- */
        /* -*-- carp -*-
        /* -*- -- Janet -*-
        /* -*- Janet -;- -*-
        // -*- iJanet -*-
        // -*- Janet; -*-
        // -*- carp-door -*-
        /* -*- model:carp -*-
        /* -*- indent-mode:carp -*-
        // -*- font:mode;Janet -*-
        // -*- mode: -*- Janet
        // -*- mode: das-carp -*-
        // -*-font:mode;mode:carp--*-
      """
      for line in invalid.split /\n/
        expect(grammar.firstLineRegex.scanner.findNextMatchSync(line)).toBeNull()

    it "recognises Vim modelines", ->
      valid = """
        vim: se filetype=carp:
        # vim: se ft=carp:
        # vim: set ft=Janet:
        # vim: set filetype=Janet:
        # vim: ft=Janet
        # vim: syntax=Janet
        # vim: se syntax=Janet:
        # ex: syntax=Janet
        # vim:ft=carp
        # vim600: ft=carp
        # vim>600: set ft=carp:
        # vi:noai:sw=3 ts=6 ft=carp
        # vi::::::::::noai:::::::::::: ft=carp
        # vim:ts=4:sts=4:sw=4:noexpandtab:ft=carp
        # vi:: noai : : : : sw   =3 ts   =6 ft  =carp
        # vim: ts=4: pi sts=4: ft=carp: noexpandtab: sw=4:
        # vim: ts=4 sts=4: ft=carp noexpandtab:
        # vim:noexpandtab sts=4 ft=carp ts=4
        # vim:noexpandtab:ft=carp
        # vim:ts=4:sts=4 ft=carp:noexpandtab:\x20
        # vim:noexpandtab titlestring=hi\|there\\\\ ft=carp ts=4
      """
      for line in valid.split /\n/
        expect(grammar.firstLineRegex.scanner.findNextMatchSync(line)).not.toBeNull()

      invalid = """
        ex: se filetype=carp:
        _vi: se filetype=carp:
         vi: se filetype=carp
        # vim set ft=klojure
        # vim: soft=carp
        # vim: clean-syntax=carp:
        # vim set ft=carp:
        # vim: setft=carp:
        # vim: se ft=carp backupdir=tmp
        # vim: set ft=carp set cmdheight=1
        # vim:noexpandtab sts:4 ft:carp ts:4
        # vim:noexpandtab titlestring=hi\\|there\\ ft=carp ts=4
        # vim:noexpandtab titlestring=hi\\|there\\\\\\ ft=carp ts=4
      """
      for line in invalid.split /\n/
        expect(grammar.firstLineRegex.scanner.findNextMatchSync(line)).toBeNull()
