const triangleSourceCode = `begin
    var n: integer := 10;
    var i := 0;
    while i < n do
    begin
        var j := 0;
        while j < n-i-1 do
        begin
            output(" ");
            j := j + 1;
        end

        j := 0;
        while j < i*2+1 do
        begin
            output("*");
            j := j + 1;
        end

        output("\\n");
        i := i + 1;
    end
end`;

export default triangleSourceCode;
