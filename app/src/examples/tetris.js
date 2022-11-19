const tetrisSourceCode = `var tetromino_t: array [4,4] of integer := [
    [0,0,0,0],
    [1,1,1,0],
    [0,1,0,0],
    [0,0,0,0]
];

var tetromino_l: array [4,4] of integer := [
    [0,1,0,0],
    [0,1,0,0],
    [0,1,1,0],
    [0,0,0,0]
];

var tetromino_z: array [4,4] of integer := [
    [0,1,0,0],
    [0,1,1,0],
    [0,0,1,0],
    [0,0,0,0]
];

var tetromino_o: array [4,4] of integer := [
    [0,0,0,0],
    [0,1,1,0],
    [0,1,1,0],
    [0,0,0,0]
];

var tetromino_i: array [4,4] of integer := [
    [0,1,0,0],
    [0,1,0,0],
    [0,1,0,0],
    [0,1,0,0]
];

var TILE_EMPTY := 0;
var TILE_BLOCK := 1;
var TILE_ALIVE := 2;

var current_tetromino: array [4,4] of integer;
var next_tetromino: array [4,4] of integer;
var current_x := 0;
var current_y := 0;
var in_game := false;
var is_over := false;

var width := 10;
var height := 24;
var board: array [24,10] of integer;

var last_tetromino_index := 0;

function init_game();
begin
    in_game := true;
    is_over := false;

    var i := 0;
    while i < height do
    begin
        var j := 0;
        while j < width do
        begin
            board[i,j] = TILE_EMPTY;
            j := j + 1;
        end
        i := i + 1;
    end

    next_tetromino := tetromino_t;
    setup_next_tetromino();
end

function on_tick();
begin
    if is_over then
        return;
    var touched_ground := is_touched_ground();
    if touched_ground then
    begin
        materialize_tetromino();
        pop_completed_rows();
        setup_next_tetromino();
        if is_overlap_ground() then
        begin
            output("Game Over");
            in_game := false;
            is_over := true;
        end;
    end
    else
        step_down();
end

function is_touched_ground() -> boolean;
begin
    var y := 0;
    while y < 4 do
    begin
        var x := 0;
        while x < 4 do
        begin
            if is_tetromino_unit_overlap_block(y, x) or is_tetromino_unit_touch_ground(y, x) then
                return true;
            x := x + 1;
        end
        y := y + 1;
    end
    return false;
end

function is_tetromino_unit_touch_ground(y: integer, x: integer) -> boolean;
begin
    var t := current_tetromino;
    if t[y,x] != 1 then
        return false;
    if current_y + y + 1 >= height then
        return true;
    if current_x + x < 0 or current_x + x >= width then
        return false;
    if board[current_y + y + 1, current_x + x] != TILE_EMPTY then
        return true;
    return false;
end

function is_tetromino_unit_overlap_block(y: integer, x: integer) -> boolean;
begin
    var t := current_tetromino;
    if t[y,x] != 1 then
        return false;
    if current_y + y >= height or current_y + y < 0 or current_x + x >= width or current_x + x < 0 then
        return true;
    if board[current_y + y, current_x + x] != TILE_EMPTY then
        return true;
    return false
end

function is_overlap_ground() -> boolean;
begin
    var y := 0;
    while y < 4 do
    begin
        var x := 0;
        while x < 4 do
        begin
            if is_tetromino_unit_overlap_block(y, x) then
                return true;
            x := x + 1;
        end
        y := y + 1;
    end
    return false;
end

function materialize_tetromino();
begin
    var t := current_tetromino;
    var y := 0;
    while y < 4 do
    begin
        var x := 0;
        while x < 4 do
        begin
            if t[y,x] = 1 then
                board[current_y + y, current_x + x] := TILE_BLOCK;
            x := x + 1;
        end
        y := y + 1;
    end
end

function pop_completed_rows();
begin
    var completed_rows_count := 0;
    var y := height - 1;
    while y >= 0 do
    begin
        var completed_rows_below := completed_rows_count;
        if is_row_completed(y) then
            completed_rows_count := completed_rows_count + 1;
        
        if completed_rows_below > 0 then
        begin
            var x := 0;
            while x < width do
            begin
                board[y + completed_rows_below, x] := board[y,x];
                board[y,x] := TILE_EMPTY;
                x := x + 1;
            end
        end

        y := y - 1;
    end
end

function is_row_completed(row: integer) -> boolean;
begin
    var x := 0;
    while x < width do
    begin
        if board[row,x] != TILE_BLOCK then
            return false;
        x := x + 1;
    end
    return true;
end

function setup_next_tetromino();
begin
    current_tetromino := next_tetromino;

    // TODO: add some randomization here.
    last_tetromino_index := (last_tetromino_index + 1) % 5;
    if last_tetromino_index = 0 then
        next_tetromino := tetromino_t;
    else if last_tetromino_index = 1 then
        next_tetromino := tetromino_l;
    else if last_tetromino_index = 2 then
        next_tetromino := tetromino_o;
    else if last_tetromino_index = 3 then
        next_tetromino := tetromino_z;
    else
        next_tetromino := tetromino_i;

    current_y := 0;
    current_x := width / 2 - 2;
end

function step_down();
begin
    current_y := current_y + 1;
end

function on_go_right();
begin
    if is_over then
        return;
    if is_hit_right_wall_or_tile() then
        return;
    current_x := current_x + 1;
end

function is_hit_right_wall_or_tile() -> boolean; 
begin
    var t := current_tetromino;
    var y := 0;
    while y < 4 do
    begin
        var x := 0;
        while x < 4 do
        begin
            if t[y,x] = 1 and is_tetromino_unit_right_wall_or_tile(y, x) then
                return true;
            x := x + 1;
        end
        y := y + 1;
    end
    return false;
end

function is_tetromino_unit_right_wall_or_tile(y: integer, x: integer) -> boolean;
begin
    var t := current_tetromino;
    if t[y,x] != 1 then
        return false;

    if current_x + x >= width-1 then
        return true;
    else if current_y + y < height and board[current_y + y,current_x + x + 1] != TILE_EMPTY then
        return true;

    return false;
end

function on_go_left();
begin
    if is_over then
        return;
    if is_hit_left_wall_or_tile() then
        return;
    current_x := current_x - 1;
end

function is_hit_left_wall_or_tile() -> boolean;
begin
    var t := current_tetromino;
    var y := 0;
    while y < 4 do
    begin
        var x := 0;
        while x < 4 do
        begin
            if t[y,x] = 1 and is_tetromino_unit_left_wall_or_tile(y, x) then
                return true;
            x := x + 1;
        end
        y := y + 1;
    end
    return false;
end

function is_tetromino_unit_left_wall_or_tile(y: integer, x: integer) -> boolean;
begin
    var t := current_tetromino;
    if t[y,x] != 1 then
        return false;

    if current_x + x <= 0 then
        return true;
    else if current_y + y < height and board[current_y + y,current_x + x - 1] != TILE_EMPTY then
        return true;

    return false;
end

function on_rotate();
begin
    if is_over then
        return;
    var initial_tetromino := current_tetromino;
    current_tetromino := rotate_tetromino(current_tetromino);
    if is_overlap_ground() then
        current_tetromino := initial_tetromino;
end

function rotate_tetromino(t: array[4,4] of integer) -> array[4,4] of integer;
begin
    return [
        [t[0,3], t[1,3], t[2,3], t[3,3]],
        [t[0,2], t[1,2], t[2,2], t[3,2]],
        [t[0,1], t[1,1], t[2,1], t[3,1]],
        [t[0,0], t[1,0], t[2,0], t[3,0]]
    ];
end

function on_smash();
begin
    if is_over then
        return;

    var current_tetromino_bottom_y: array[4] of integer := [0, 0, 0, 0];
    var x := 0;
    while x < 4 do
    begin
        var y := 0;
        while y < 4 do
        begin
            if current_tetromino[y,x] = 1 then
                current_tetromino_bottom_y[x] := current_y + y;
            y := y + 1;
        end
        x := x + 1;
    end

    var ground_y: array[4] of integer := [height, height, height, height];

    var x := 0;
    while x < 4 do
    begin
        if current_x + x >= width or current_x + x < 0 then
        begin
            ground_y[x] := -1;
            x := x + 1;
            continue;
        end

        var y := current_tetromino_bottom_y[x];
        while y < height do
        begin
            if board[y,current_x + x] != TILE_EMPTY then
            begin
                ground_y[x] := y;
                break;
            end
            y := y + 1;
        end
        x := x + 1;
    end

    var step_to_ground := height;
    var x := 0;
    while x < 4 do
    begin
        var y := 3;
        while y >= 0 do
        begin
            if current_tetromino[y,x] = 1 then
            begin
                var new_step_to_ground := ground_y[x] - (current_y + y) - 1;
                if step_to_ground > new_step_to_ground then
                    step_to_ground := new_step_to_ground;
            end
            y := y - 1;
        end
        x := x + 1;
    end

    current_y := current_y + step_to_ground;
    materialize_tetromino();
    pop_completed_rows();
    setup_next_tetromino();
end

function on_keydown(key: string);
begin
    if key = "ArrowRight" then
        on_go_right();
    else if key = "ArrowLeft" then
        on_go_left();
    else if key = "ArrowUp" then
        on_rotate();
    else if key = "ArrowDown" then
        on_smash();
end

var last_tick: integer := 0;
function on_update();
begin
    var t := system_time_millis();
    if t - last_tick > 500 then
    begin
        last_tick := t;
        on_tick();
    end

    on_draw();
end

var render_frame: array [24,10] of integer;

function on_draw();
begin
    var block_size := 15;

    var min_width := block_size * width;
    var min_height := block_size * height;
    var screen_width := get_width();
    var screen_height := get_height();
    if screen_width < min_width or screen_height < min_height then
    begin
      output("Cannot draw, screen too small\\n");  
      return;
    end

    var y := 0;
    while y < height do
    begin
        var x := 0;
        while x < width do
        begin
            render_frame[y,x] := board[y,x];
            x := x + 1;
        end
        y := y + 1;
    end

    if !is_over then
    begin
        var y := 0;
        while y < 4 do
        begin
            var x := 0;
            while x < 4 do
            begin
                var xx := current_x + x;
                var yy := current_y + y;
                if current_tetromino[y,x] = 1 and xx >= 0 and xx < width and yy >= 0 and yy < height then
                begin
                    render_frame[yy,xx] := TILE_ALIVE;
                end
                x := x + 1;
            end
            y := y + 1;
        end
    end

    var y := 0;
    while y < height do
    begin
        var x := 0;
        while x < width do
        begin
            var color := "#fff";
            if render_frame[y,x] = TILE_EMPTY then
                color := "#f0f0f0";
            else if render_frame[y,x] = TILE_BLOCK then
                color := "#0f0f0f";
            else if render_frame[y,x] = TILE_ALIVE then
                color := "#f000f0";

            draw_rect(x*block_size, y*block_size, block_size, block_size, color);

            x := x + 1;
        end
        y := y + 1;
    end
end

begin
    init_game();

    register_on_keydown(on_keydown);
    register_on_update(on_update);
end
`;

export default tetrisSourceCode;
