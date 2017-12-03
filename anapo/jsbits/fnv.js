function h$stringFnvHash(hash, str) {
  var len = str.length;
  if (len > 0) {
    for (var i = 0; i < len; i++) {
      hash = h$mulInt32(hash, 16777619) ^ str.charCodeAt(i);
    }
  }
  return hash;
}
