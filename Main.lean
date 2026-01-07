import DesignPatternsInLean4

def main : IO Unit := do
  IO.println "=== Design Patterns in Lean4 ==="
  IO.println ""
  IO.println "--- Strategy Pattern ---"
  strategyDemo
  IO.println ""
  IO.println "--- Composite Pattern ---"
  compositeDemo
