const gameOfLifeSourceCode = `var size: integer := 40;
var board: array[40,40] of integer;
var board_next: array[40,40] of integer;
var is_running: boolean := false;

var LIFE: integer := 1;
var DEAD: integer := 0;

function on_mouse_click(x: integer, y: integer)
begin
  var w := get_width();
  var h := get_height();
  var board_size := w;
  if h < board_size then
    board_size := h;

  // add small padding
  board_size := board_size - 20;

  var offset_x := (w - board_size) / 2;
  var offset_y := (h - board_size) / 2;
  var block_size := board_size / size;
  var select_x := (x - offset_x) / block_size;
  var select_y := (y - offset_y) / block_size;
  if select_x >= 0 and select_x < size and select_y >= 0 and select_y < size then
  begin
    board[select_x,select_y] := 1 - board[select_x,select_y];
  end
end

var mouse_x: integer;
var mouse_y: integer;
function on_mouse_move(x: integer, y: integer)
begin
    mouse_x := x;
    mouse_y := y;
end

function on_key_down(key: string)
begin
    if key = "Enter" then
        is_running := !is_running;
end

var last_tick: integer := 0;
function on_update()
begin
    var t := system_time_millis();
    if t - last_tick > 10 then
    begin
        last_tick := t;
        on_tick();
    end

    draw_board();
end

function on_tick()
begin
  // Rules from: https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life

  if !is_running then
    return;

  var x := 0;
  while x < size do
  begin
    var y := 0;
    while y < size do
    begin
        var life_neighbors := 0;

        if x > 0 and y > 0 and board[x-1,y-1] = LIFE then
            life_neighbors := life_neighbors + 1;
        if y > 0 and board[x,y-1] = LIFE then
            life_neighbors := life_neighbors + 1;
        if x < size - 1 and y > 0 and board[x+1,y-1] = LIFE then
            life_neighbors := life_neighbors + 1;
        if x < size - 1 and board[x+1,y] = LIFE then
            life_neighbors := life_neighbors + 1;
        if x < size - 1 and y < size - 1 and board[x+1,y+1] = LIFE then
            life_neighbors := life_neighbors + 1;
        if y < size - 1 and board[x,y+1] = LIFE then
            life_neighbors := life_neighbors + 1;
        if x > 0 and y < size - 1 and board[x-1,y+1] = LIFE then
            life_neighbors := life_neighbors + 1;
        if x > 0 and board[x-1,y] = LIFE then
            life_neighbors := life_neighbors + 1;

        if board[x,y] = LIFE then
        begin
            if life_neighbors < 2 then
                board_next[x,y] := DEAD;
            else if life_neighbors < 4 then
                board_next[x,y] := LIFE;
            else
                board_next[x,y] := DEAD;
        end
        else
        begin
            if life_neighbors = 3 then
                board_next[x,y] := LIFE;
            else
                board_next[x,y] := DEAD;
        end

        y := y + 1;
    end
    x := x + 1;
  end

  x := 0;
  while x < size do
  begin
    var y := 0;
    while y < size do
    begin
        board[x,y] := board_next[x,y];
        y := y + 1;
    end
    x := x + 1;
  end
end

function draw_board()
begin
  var w := get_width();
  var h := get_height();
  var board_size := w;
  if h < board_size then
    board_size := h;

  // add small padding
  board_size := board_size - 20;

  var offset_x := (w - board_size) / 2;
  var offset_y := (h - board_size) / 2;
  var block_size := board_size / size;

  draw_rect(0, 0, w, h, "#000", "#000");

  var x := 0;
  while x < size do
  begin
    var y := 0;
    while y < size do
    begin
        if board[x,y] = LIFE then
            draw_rect(offset_x + x * block_size, offset_y + y * block_size, block_size, block_size, "#000", "#fff");
        y := y + 1;
    end
    x := x + 1;
  end

  var select_x := (mouse_x - offset_x) / block_size;
  var select_y := (mouse_y - offset_y) / block_size;
  if select_x >= 0 and select_x < size and select_y >= 0 and select_y < size then
  begin
    var color := "#000";
    if board[select_x,select_y] = LIFE then
        color := "#fff";
    draw_rect(offset_x + select_x * block_size, offset_y + select_y * block_size, block_size, block_size, "#fff", color);
  end
end

begin
    output("https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life\\n");
    output("Use enter to play/pause\\n");
    output("When paused, click the board to set the cell into life or dead\\n");

    board[20,20] := LIFE;
    board[21,20] := LIFE;
    board[21,21] := LIFE;
    board[25,21] := LIFE;
    board[26,21] := LIFE;
    board[27,21] := LIFE;
    board[26,19] := LIFE;

    register_on_mouse_move(on_mouse_move);
    register_on_mouse_click(on_mouse_click);
    register_on_keydown(on_key_down);
    register_on_update(on_update);
end
`;

export default gameOfLifeSourceCode;
