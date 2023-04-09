# LuaSyntaxTree
A Lua-AST generator written in Lua 5.4.4

> ⚠️project does not yet contain a production build.

## Motivation
Not much motivation other than it's been a topic i've wanted to explore for a while, I eventually want to write up my own interpreter using resources such as this AST.

## Tips'N Tricks
Project supports .VSCODE build tasks, please use these tasks to build & run tests on the project.

## Example
```lua
local AST = SyntaxTree.new("print'Hello, World!'")
```

