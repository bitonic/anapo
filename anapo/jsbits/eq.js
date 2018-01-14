function h$anapoJsEq(x, y) {
  if (typeof x === "string" && typeof y === "string") {
    return x === y;
  }
  if (typeof x === "boolean" && typeof y === "boolean") {
    return x === y;
  }
  if (typeof x === "number" && typeof y === "number") {
    return x === y;
  }
  return false;
}
