local SyntaxTree = require("SyntaxTree")

-- https://stackoverflow.com/a/54593224
-- this version of the about removes the 'depth' variable and watches for recursion in tables.
function print_table(tbl, tracked, index)
    tracked = tracked or { }
    index = index or 0;

    if tracked[tbl] then
        print(string.rep(' ', index) .. "<recursive_table>")

        return;
    else
        tracked[tbl] = true
    end

    for key, value in pairs(tbl) do
        if (key and type(key) == "number" or type(key) == "string") then
            key = string.format("[\"%s\"]", key);

            if (type(value) == "table") then
                if (next(value)) then
                    print(string.rep(' ', index)..key.." = {");
                    print_table(value, tracked, index + 4);
                    print(string.rep(' ', index).."},");
                else
                    print(string.rep(' ', index)..key.." = {},");
                end
            else
                if (type(value) == "string") then
                    value = string.format("\"%s\"", value);
                else
                    value = tostring(value);
                end

                print(string.rep(' ', index)..key.." = "..value..",");
            end
        end
    end
end

print_table(SyntaxTree.new([[
local a, b, c = true, false, function(c, d, e, f, g)
    return 123
end
]]))