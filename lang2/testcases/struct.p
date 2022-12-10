pub struct Struct1 {
  A: int32,
}

pub struct Struct2 {
  B: Struct1,
  C: *Struct1,
  D: *int32,
}

func main() {
  var a: Struct1;
  a.A = 2;
  a.A = a.A + 1;
}
