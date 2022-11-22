const snakeSourceCode = `var size := 40;
var board: array[40,40] of integer;

var EMPTY := 0;
var FOOD := 1;
var BODY := 2;
var HEAD := 3;

var is_game_over := false;

var snake_size: integer;
var snake_direction: integer;
var snake_x: array[1600] of integer;
var snake_y: array[1600] of integer;
var snake_is_growing := false;

var DIR_RIGHT := 0;
var DIR_DOWN := 1;
var DIR_LEFT := 2;
var DIR_UP := 3;

var food_x: integer;
var food_y: integer;

function init_game()
begin
  snake_x[0] := size / 2 - 1;
  snake_y[0] := size / 2;
  snake_x[1] := size / 2;
  snake_y[1] := size / 2;
  snake_x[2] := size / 2 + 1;
  snake_y[2] := size / 2;
  snake_size := 3;
  snake_direction := DIR_RIGHT;

  board[size/2-1, size/2] := BODY;
  board[size/2, size/2] := BODY;
  board[size/2+1, size/2] := HEAD;

  randomize_food();
end

function on_tick()
begin
  if is_game_over then
    return;

  if !snake_is_growing then
  begin
    board[snake_x[0], snake_y[0]] := EMPTY;

    var i := 0;
    while i < snake_size-1 do
    begin
      snake_x[i] := snake_x[i + 1];
      snake_y[i] := snake_y[i + 1];
      i := i + 1;
    end
  end

  var snake_head_x := snake_x[snake_size-1];
  var snake_head_y := snake_y[snake_size-1];
  board[snake_head_x, snake_head_y] := BODY;

  if snake_direction = DIR_RIGHT then
    snake_head_x := snake_head_x + 1;
  else if snake_direction = DIR_DOWN then
    snake_head_y := snake_head_y + 1;
  else if snake_direction = DIR_LEFT then
    snake_head_x := snake_head_x - 1;
  else
    snake_head_y := snake_head_y - 1;

  snake_head_x := ((snake_head_x % size) + size) % size;
  snake_head_y := ((snake_head_y % size) + size) % size;

  if board[snake_head_x, snake_head_y] = BODY then
  begin
    output("Game Over!\\n");
    is_game_over := true;
  end

  board[snake_head_x, snake_head_y] := HEAD;

  if snake_is_growing then
  begin
    snake_size := snake_size + 1;
    snake_is_growing := false;
  end

  snake_x[snake_size-1] := snake_head_x;
  snake_y[snake_size-1] := snake_head_y;
  
  if snake_head_x = food_x and snake_head_y = food_y then
  begin
    randomize_food();
    delay := delay - 2;
    if delay < 10 then
      delay := 10;
    snake_is_growing := true;
  end
end

function randomize_food()
begin
  var available_x: array[40] of integer;
  var len := 0;

  // TODO: handle if the whole block is occupied

  var x := 0;
  while x < size do
  begin
    var y := 0;
    var has_empty := false;
    while y < 40 do
    begin
      if board[x,y] = EMPTY then
      begin
        has_empty := true;
        break;
      end
      y := y + 1;
    end

    if has_empty then
    begin
      available_x[len] := x;
      len := len + 1;
    end
    x := x + 1;
  end

  food_x := available_x[0];
  if len > 1 then
    food_x := available_x[next_random() % len];

  var available_y: array[40] of integer;
  len := 0;
  var y := 0;
  while y < 40 do
  begin
    if board[food_x,y] = EMPTY then
    begin
      available_y[len] := y;
      len := len + 1;
    end
    y := y + 1;
  end

  food_y := available_y[0];
  if len > 1 then
    food_y := available_y[next_random() % len];

  board[food_x, food_y] := FOOD;
end

var seed: integer;
function next_random() -> integer
begin
  seed := (seed * 75 + 74) % 65537;
  return seed;
end

var last_ts := 0;
var delay := 100;
function on_update()
begin
  var t := system_time_millis();
  if t - last_ts > delay then
  begin
    last_ts := t;
    on_tick();
  end

  draw();
end

function on_keydown(key: string)
begin
  if key = "ArrowDown" and snake_direction != DIR_UP then
    snake_direction := DIR_DOWN;
  else if key = "ArrowRight" and snake_direction != DIR_LEFT then
    snake_direction := DIR_RIGHT;
  else if key = "ArrowUp" and snake_direction != DIR_DOWN then
    snake_direction := DIR_UP;
  else if key = "ArrowLeft" and snake_direction != DIR_RIGHT then
    snake_direction := DIR_LEFT;
end

function draw()
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

  draw_rect(offset_x, offset_y, block_size * size, block_size * size, "#000", "#fff");

  var x := 0;
  while x < size do
  begin
    var y := 0;
    while y < size do
    begin
        var color := "#f0f0f0";
        if board[x,y] = HEAD then
          color := "#f000f0";
        else if board[x,y] = BODY then
          color := "#000000";
        else if board[x,y] = FOOD then
          color := "#f05000";
        draw_rect(offset_x + x * block_size, offset_y + y * block_size, block_size, block_size, "#fff", color);
        y := y + 1;
    end
    x := x + 1;
  end
end

begin
  seed := unix_time_millis();
  init_game();

  register_on_update(on_update);
  register_on_keydown(on_keydown);
end`;

export default snakeSourceCode;
