local SyntaxTree = { }

-----------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------

SyntaxTree.Interface = { }
SyntaxTree.Parser = { }
SyntaxTree.Node = { }
SyntaxTree.Lexer = { }

SyntaxTree.Settings = {}

SyntaxTree.Lexer.EOS = "EndOfSource"
SyntaxTree.Lexer.Lexemes = {
    ["Keyword"] = { "^and", "^break", "^do", "^else", "^elseif", "^end", "^for", "^function", "^if", "^in", "^local", "^nil", "^not", "^while", "^repeat", "^return", "^then", "^self", "^until", "^or", "^false", "^true" },
    ["BinaryOperator"] = { '^+', '^-', '^*', '^/', '^%%', '^<', "^<=", '^>', "^>=", "^==", "^~=", "^%.%." },
    ["UnaryOperator"] = { '^-', '^not', '^#' },
    ["StringMatch"] = { "^(['\"])%1", [[^(['"])(\*)%2%1]], [[^(['"]).-[^\](\*)%2%1]], "^(['\"]).-.*", "^%[(=*)%[.-%]%1%]", "^%[%[.-.*" },
    ["NumberMatch"] = { "^0x[%da-fA-F]+", "^%d+%.?%d*[eE][%+%-]?%d+", "^%d+%.?%d*" },
    ["IdentifierMatch"] = { "^[%a_][%w_]*" },
    ["SpaceMatch"] = { "^%s+" },
    ["CommentMatch"] = { "^%-%-%[(=*)%[.-%]%1%]", "^%-%-%[%[.-.*", "^%-%-.-\n", "^%-%-.-.*", },
    ["UnknownMatch"] = { "^%.%.%.", "^." }
}

SyntaxTree.Lexer.Priority = {
    "Keyword",
    "BinaryOperator",
    "UnaryOperator",
    "StringMatch",
    "NumberMatch",
    "IdentifierMatch",
    "SpaceMatch",
    "CommentMatch",
    "UnknownMatch"
}

SyntaxTree.Settings.Verbose = true

-----------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------

function SyntaxTree.Node:SetParent(parentNode)
    if self._parent then
        local index = table.find(self._parent, self)

        if index then
            table.remove(self._parent, index)
        end
    end

    self._parent = parentNode

    if parentNode then
        table.insert(parentNode._children, parentNode)
    end
end

function SyntaxTree.Node.new(nodeType, nodeParent)
    local nodeObject = setmetatable({
        type = nodeType,
        _parent = nodeParent,
        _children = { }
    }, { __index = SyntaxTree.Node })

    if nodeParent then
        table.insert(nodeParent._children, nodeObject)
    end

    return nodeObject
end

-----------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------

function SyntaxTree.Parser:parseVariableList(lexer)
    local tableNode = SyntaxTree.Node.new("VariableList")
    local variableList = { }

    while true do
        if lexer:peek() ~= "IdentifierMatch" then
            break
        end

        table.insert(variableList, select(2, lexer:next()))

        if select(2, lexer:peek()) == "," then
            lexer:next()
        else
            break
        end
    end

    tableNode.list = variableList

    return variableList
end

function SyntaxTree.Parser:parseExpressionList(lexer)
    local tableNode = SyntaxTree.Node.new("ExpressionList")
    local expressionList = { }

    while true do
        table.insert(expressionList, self:parseExpression(lexer))

        if select(2, lexer:peek()) == "," then
            lexer:next()
        else
            break
        end
    end

    tableNode.list = expressionList

    return expressionList
end

function SyntaxTree.Parser:parseTableConstructor(lexer)
    assert(select(2, lexer:next()) == "{", "Expected '{' when accessing table constructor")

    local tableNode = SyntaxTree.Node.new("TableConstructor")
    local tableContainments = { }

    while true do
        local lexemeType, lexemeSource = lexer:next()
        local key, value

        if lexemeSource == "[" then
            key = self:parseExpression(lexer)

            assert(select(2, lexer:next()) == "]", "Expected ']' when closing table key constructor")
            assert(select(2, lexer:next()) == "=", "Expected '=' when closing table key constructor")

            value = self:parseExpression(lexer)
        elseif lexemeType == "IdentifierMatch" then
            key = lexemeSource

            assert(select(2, lexer:next()) == "=", "Expected '=' when closing table key constructor")

            value = self:parseExpression(lexer)
        else
            key = #tableContainments + 1

            value = self:parseExpression(lexer)
        end

        tableContainments[key] = value
        lexemeType, lexemeSource = lexer:peek()

        if (lexemeSource == ",") or (lexemeSource == ";") then
            lexer:skip()
        else
            break
        end
    end

    tableNode.data = tableContainments

    assert(select(2, lexer:next()) == "}", "Expected '}' when closing table constructor")

    return tableNode
end

function SyntaxTree.Parser:parseExpression(lexer)
    local lexemeType, lexemeSource = lexer:peek()
    local expressionNode = SyntaxTree.Node.new("Expression")

    if lexemeSource == "{" then
        expressionNode.expressionType = "TableConstructor"
        expressionNode.value = self:parseTableConstructor(lexer)

        return expressionNode
    end

    lexemeType, lexemeSource = lexer:next()

    if lexemeType == "NumberMatch" then
        expressionNode.expressionType = "Number"
        expressionNode.value = lexemeSource
    elseif lexemeType == "StringMatch" then
        expressionNode.expressionType = "String"
        expressionNode.value = lexemeSource
    elseif lexemeType == "UnknownMatch" then
        expressionNode.expressionType = "Varadic"
        expressionNode.value = lexemeSource
    elseif lexemeType == "IdentifierMatch" then
        local nextLexemeType, nextLexemeSource = lexer:peek()

        if nextLexemeType == "StringMatch" or nextLexemeSource == "(" or nextLexemeSource == "{" then
            expressionNode.expressionType = "AnonymousFunction"
            expressionNode.value = self:parseFunctionCall(lexer, lexemeSource)
        else
            expressionNode.expressionType = "Reference"
            expressionNode.value = lexemeSource
        end
    elseif lexemeType == "Keyword" then
        if lexemeSource == "function" then
            assert(select(2, lexer:next()) == "(", "Expected '(' when opening function")

            local argumentsList = { }

            if select(2, lexer:peek()) ~= ")" then
                argumentsList = self:parseExpressionList(lexer)
            end

            assert(select(2, lexer:next()) == ")", "Expected ')' when closing function")

            local functionBlock = self:parseBlock(lexer)

            expressionNode.expressionType = "Function"
            expressionNode.arguments = argumentsList
            expressionNode.value = functionBlock
        elseif (lexemeSource == "nil" and lexemeSource) or (lexemeSource == "true" and lexemeSource) or (lexemeSource == "false" and lexemeSource) then
            expressionNode.expressionType = "Keyword"
            expressionNode.value = lexemeSource
        else
            error("unexpected keyword when parsing expression: " .. lexemeSource)
        end
    end

    return expressionNode
end

function SyntaxTree.Parser:parseFunctionCall(lexer, identifier)
    local lexemeType, lexemeSource = lexer:peek()

    local functionCallNode = SyntaxTree.Node.new("FunctionCall", lexer.nodes[lexer.scope])
    local functionArgs = { }

    if lexemeType == "StringMatch" then
        functionArgs = { self:parseExpression(lexer) }
    elseif lexemeSource == "(" then
        lexer:skip()
        functionArgs = self:parseExpressionList(lexer)

        assert(select(2, lexer:next()) == ")", "Expected ')' bracket to close function call")
    else
        error("Expected '(' when opening new function")
    end

    functionCallNode.name = identifier
    functionCallNode.args = functionArgs

    return functionCallNode
end

function SyntaxTree.Parser:parseFunction(lexer)
    local functionNode = SyntaxTree.Node.new("Block", lexer.nodes[lexer.scope - 1])
    local lexemeType, lexemeSource = lexer:next()

    assert(lexemeType == "IdentifierMatch", "Missing function name identifier")
    assert(select(2, lexer:next()) == "(", "Expected '(' when opening function")

    local argumentsList = self:parseExpressionList(lexer)

    assert(select(2, lexer:next()) == ")", "Expected ')' bracket to close function call")

    local blockNode = self:parseBlock(lexer)

    functionNode.block = blockNode
    functionNode.name = lexemeSource
    functionNode.arguments = argumentsList

    return functionNode
end

function SyntaxTree.Parser:parseStatement(lexer)
    
end

function SyntaxTree.Parser:parseBlock(lexer)
    if SyntaxTree.Settings.Verbose then
        print(string.format("SyntaxTree.Parser:parseBlock() => Entering a new Lua Block at %d scope", lexer.scope))
    end

    local blockNode = SyntaxTree.Node.new("Block", lexer.nodes[lexer.scope - 1])

    lexer.scope = lexer.scope + 1
    lexer.nodes[lexer.scope] = blockNode

    local lexemeType, lexemeSource = lexer:next()

    while lexemeType ~= SyntaxTree.Lexer.EOS and lexemeSource ~= "end" do
        if SyntaxTree.Settings.Verbose then
            print(string.format("SyntaxTree.Parser:parseBlock() => Main Source, %s, %s, %d", lexemeType, lexemeSource, lexer.scope))
        end

        lexemeType, lexemeSource = lexer:active()

        if lexemeType == "IdentifierMatch" then
            local nextLexemeType, nextLexemeSource = lexer:peek()

            if nextLexemeType == "StringMatch" or nextLexemeSource == "(" or nextLexemeSource == "{" then
                self:parseFunctionCall(lexer, lexemeSource)
            end
        elseif lexemeType == "Keyword" then
            local nextLexemeType, nextLexemeSource = lexer:peek()

            if nextLexemeType == "IdentifierMatch" then
                local localNode = SyntaxTree.Node.new("LocalDefinition", blockNode)
                local variableList, expressionList = self:parseVariableList(lexer), { }
                local _, nextLexemeSource = lexer:peek()

                if nextLexemeSource == "=" then
                    lexer:skip()

                    expressionList = self:parseExpressionList(lexer)
                end

                localNode.expressionList = expressionList
                localNode.variableList = variableList
            elseif nextLexemeType == "Keyword" then
                assert(nextLexemeSource == "function", "Unexpected keyword at sourced block")

                lexer:skip()

                self:parseFunction(lexer):SetParent(blockNode)
            end
        end

        lexemeType, lexemeSource = lexer:next()
    end

    if SyntaxTree.Settings.Verbose then
        print(string.format("SyntaxTree.Parser:parseBlock() => Main Source, %s, %d", lexer:active(), lexer.scope))
        print(string.format("SyntaxTree.Parser:parseBlock() => Exiting a new Lua Block at %d scope", lexer.scope))
    end

    lexer.nodes[lexer.scope] = nil
    lexer.scope = lexer.scope - 1

    return blockNode
end

function SyntaxTree.Parser:parseState(lexer)
    return self:parseBlock(lexer)
end

-----------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------

--[=[
    @Class SyntaxTree.Lexer

    The SyntaxTree Lexer helps to resolve words, tokens and unique characters that'll ideally paint the picture of a working Lua block
    It is important to note that the Lexer works by always being a step ahead of the latest - thus allowing us to implement a `Peek` functionality.

    tokenStack Object: {
        [1] = <lexeme type>
        [2] = <source>
    }
]=]--

--[=[
    Initialize the lexer, declare state & internal variables needed for computation of a string.

    @within SyntaxTree.Lexer
]=]--
function SyntaxTree.Lexer:init()
    self.tokenStack = {
        --[[
            [1] = <lastToken>
            [2] = <currentToken>
            [3] = <nextToken>
        ]]--
    }

    self.scope = 0
    self.index = 0

    self.nodes = { }

    self:next() -- instantiate 'nextToken' in token stack
end

--[=[
    Peek into the next token avaliable within the 'tokenStack'

    @within SyntaxTree.Lexer
]=]--
function SyntaxTree.Lexer:peek()
    if not self.tokenStack[3] then
        return SyntaxTree.Lexer.EOS
    end

    return table.unpack(self.tokenStack[3])
end

--[=[
    Process the 'tokenStack' & push all tokens into the next avaliable token

    @within SyntaxTree.Lexer
]=]--
function SyntaxTree.Lexer:next()
    if self.index > #self.source then
        if self.tokenStack[3] ~= nil then
            self.tokenStack[1] = self.tokenStack[2]
            self.tokenStack[2] = self.tokenStack[3]
            self.tokenStack[3] = nil

            return table.unpack(self.tokenStack[2])
        elseif self.tokenStack[2] ~= nil then
            self.tokenStack[1] = self.tokenStack[2]
            self.tokenStack[2] = nil
        end

		return SyntaxTree.Lexer.EOS
	end

    self.tokenLast = self.tokenActive
	self.tokenActive = self.tokenHead

    self.tokenStack[1] = self.tokenStack[2]
    self.tokenStack[2] = self.tokenStack[3]

    for _, lexemeType in next, SyntaxTree.Lexer.Priority do
        local lexemeQueries = SyntaxTree.Lexer.Lexemes[lexemeType]

        for _, lexemeMatch in next, lexemeQueries do
            local matchStartIndex, matchEndIndex = string.find(self.source, lexemeMatch, self.index)

            if matchStartIndex and matchEndIndex then
                self.index = matchEndIndex + 1

                if lexemeType == "SpaceMatch" then
                    return self:next()
                end

                self.tokenStack[3] = { lexemeType, string.sub(self.source, matchStartIndex, matchEndIndex) }

                if SyntaxTree.Settings.Verbose then
                    print(string.format("SyntaxTree.Lexer:next() => %s, %d - %d, %s, %s", lexemeType, matchStartIndex, matchEndIndex, lexemeMatch, self.tokenStack[3][2]))
                end

                if self.tokenStack[2] then
                    return table.unpack(self.tokenStack[2])
                else
                    return
                end
            end
        end
    end
end

--[=[
    Get the current token that is being processed

    @within SyntaxTree.Lexer
]=]--
function SyntaxTree.Lexer:active()
    if not self.tokenStack[2] then
        return SyntaxTree.Lexer.EOS
    end

    return table.unpack(self.tokenStack[2])
end

--[=[
    Match the 'source' of the current active token to another string

    @within SyntaxTree.Lexer
    @param source string
]=]--
function SyntaxTree.Lexer:match(source)
    return select(2, self:active()) == source
end

--[=[
    Peek into the last token avaliable within the 'tokenStack'

    @within SyntaxTree.Lexer
]=]--
function SyntaxTree.Lexer:last()
    return table.unpack(self.tokenStack[1])
end

--[=[
    Skip over tokens

    @within SyntaxTree.Lexer
    @param count number
]=]--
function SyntaxTree.Lexer:skip(count)
    for _ = 1, count or 1 do
        self:next()
    end
end

-----------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------

function SyntaxTree.Interface.new(source)
    local state = setmetatable({ source = source }, { __index = SyntaxTree.Lexer })

    state:init()

    return SyntaxTree.Parser:parseState(state)
end

return SyntaxTree.Interface