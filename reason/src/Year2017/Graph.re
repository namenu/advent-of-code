module MakeGraph = (M: Belt.Id.Comparable) => {
  module V = Belt.Set;

  type vertex = M.t;
  type vertices = V.t(vertex, M.identity);

  type t('a) = {
    nodes: vertices,
    neighbors: vertex => vertices,
  };

  let verticesFromArray = V.fromArray(_, ~id=(module M));

  let dfs = (g: t('a), startNode): vertices => {
    let rec visit = (curNode, visited: vertices) => {
      g.neighbors(curNode)
      ->V.reduce(V.add(visited, curNode), (visited, nextNode) => {
          V.has(visited, nextNode) ? visited : visit(nextNode, visited)
        });
    };

    let empty = V.make(~id=(module M));
    visit(startNode, empty);
  };

  let groups = (g): list(vertices) => {
    let rec visitAll = (unvisited: vertices): list(vertices) => {
      switch (unvisited->V.toList) {
      | [] => []
      | [u, ..._] =>
        let visited = dfs(g, u);
        [visited, ...visitAll(unvisited->V.removeMany(visited->V.toArray))];
      };
    };

    let unvisited = g.nodes;
    visitAll(unvisited);
  };
};
