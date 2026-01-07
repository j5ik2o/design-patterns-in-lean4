/-
  Composite Pattern - Inductive-based Implementation
  Rust の enum を使ったアプローチに相当。Lean4 では inductive type を使用。
-/

namespace Composite.InductiveBase

/-- ファイルを表す構造体 -/
structure File where
  name : String
  size : Nat
  deriving Repr, BEq

/-- エントリ（ファイルまたはディレクトリ）を表す帰納型 -/
inductive Entry where
  | file : File → Entry
  | directory : String → List Entry → Entry
  deriving Repr

namespace Entry

/-- エントリの名前を取得 -/
def getName : Entry → String
  | file f => f.name
  | directory name _ => name

/-- エントリのサイズを取得（ディレクトリは再帰的に合計） -/
def getSize : Entry → Nat
  | file f => f.size
  | directory _ entries => entries.foldl (fun acc e => acc + e.getSize) 0

/-- プレフィックス付きで表示用文字列を生成 -/
partial def printLineWithPrefix (entry : Entry) (prefix_ : String) : String :=
  match entry with
  | file f => s!"{prefix_}/{f.name} ({f.size})"
  | directory name entries =>
    let header := s!"{prefix_}/{name}"
    let children := entries.map (fun e => printLineWithPrefix e s!"{prefix_}/{name}")
    header :: children |>.foldl (fun acc s => acc ++ "\n" ++ s) ""

/-- エントリを文字列として表示 -/
def printLine (entry : Entry) : String :=
  printLineWithPrefix entry ""

instance : ToString Entry where
  toString e := s!"{e.getName} ({e.getSize})"

end Entry

/-- ディレクトリにエントリを追加 -/
def addEntry (dir : Entry) (entry : Entry) : Entry :=
  match dir with
  | Entry.directory name entries => Entry.directory name (entries ++ [entry])
  | _ => dir  -- ファイルには追加できない

end Composite.InductiveBase
