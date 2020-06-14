function filter(func, list, result = []) {
  return list.length === 0
    ? result
    : filter(
        func,
        list.slice(1),
        func(list[0]) ? [...result, list[0]] : [...result]
      );
}

console.log(filter((x) => x % 2 === 0, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]));
