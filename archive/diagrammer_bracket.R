# Install DiagrammeR if you haven't already
if (!requireNamespace("DiagrammeR", quietly = TRUE)) {
  install.packages("DiagrammeR")
}

library(DiagrammeR)

# Create a tournament bracket
grViz("
digraph TournamentBracket {
  rankdir=LR; // Horizontal layout

  // Nodes for the bracket
  node [shape=box style=filled color=lightblue];

  A1 [label='Team 1'];
  A2 [label='Team 2'];
  A3 [label='Team 3'];
  A4 [label='Team 4'];
  A5 [label='Team 5'];
  A6 [label='Team 6'];
  A7 [label='Team 7'];
  A8 [label='Team 8'];

  B1 [label='Winner 1'];
  B2 [label='Winner 2'];
  B3 [label='Winner 3'];
  B4 [label='Winner 4'];

  C1 [label='Semi Finalist 1'];
  C2 [label='Semi Finalist 2'];

  D1 [label='Champion'];

  // Edges to connect rounds
  A1 -> B1;
  A2 -> B1;

  A3 -> B2;
  A4 -> B2;

  A5 -> B3;
  A6 -> B3;

  A7 -> B4;
  A8 -> B4;

  B1 -> C1;
  B2 -> C1;

  B3 -> C2;
  B4 -> C2;

  C1 -> D1;
  C2 -> D1;
}
")
