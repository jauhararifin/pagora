func main() {
  var sum: int64 = 0;
  var i: int64 = 1;
  while i <= 1000 {
    if i % 3 == 0 || i % 5 == 0 {
      sum = sum + i;
    }
    i = i + 1;
  }
}
