/-
  Composite Pattern - Class-based Implementation
  Rust の trait を使ったアプローチに相当。Lean4 では type class を使用。
-/

namespace Composite.ClassBase

/-- エントリの基本インターフェース（trait相当） -/
class EntryBase (α : Type) where
  getName : α → String
  getSize : α → Nat
  printLineWithPrefix : α → String → String

/-- ファイルを表す構造体 -/
structure File where
  name : String
  size : Nat
  deriving Repr, BEq

namespace File

instance : EntryBase File where
  getName f := f.name
  getSize f := f.size
  printLineWithPrefix f prefix_ := s!"{prefix_}/{f.name} ({f.size})"

instance : ToString File where
  toString f := s!"{f.name} ({f.size})"

end File

/-- エントリを統一的に扱うための存在型ラッパー -/
inductive EntryWrapper where
  | file : File → EntryWrapper
  | directory : String → List EntryWrapper → EntryWrapper
  deriving Repr

namespace EntryWrapper

def getName : EntryWrapper → String
  | file f => f.name
  | directory name _ => name

def getSize : EntryWrapper → Nat
  | file f => f.size
  | directory _ entries => entries.foldl (fun acc e => acc + e.getSize) 0

partial def printLineWithPrefix (e : EntryWrapper) (prefix_ : String) : String :=
  match e with
  | file f => EntryBase.printLineWithPrefix f prefix_
  | directory name entries =>
    let header := s!"{prefix_}/{name}"
    let children := entries.map (fun child => child.printLineWithPrefix s!"{prefix_}/{name}")
    String.intercalate "\n" (header :: children)

def printLine (e : EntryWrapper) : String :=
  e.printLineWithPrefix ""

instance : ToString EntryWrapper where
  toString e := s!"{e.getName} ({e.getSize})"

instance : EntryBase EntryWrapper where
  getName := getName
  getSize := getSize
  printLineWithPrefix := printLineWithPrefix

end EntryWrapper

/-- ディレクトリを表す構造体 -/
structure Directory where
  name : String
  entries : List EntryWrapper := []
  deriving Repr

namespace Directory

/-- ディレクトリにエントリを追加 -/
def add (dir : Directory) (entry : EntryWrapper) : Directory :=
  { dir with entries := dir.entries ++ [entry] }

/-- ディレクトリのサイズを取得 -/
def getSize (dir : Directory) : Nat :=
  dir.entries.foldl (fun acc e => acc + EntryWrapper.getSize e) 0

/-- EntryWrapperに変換 -/
def toWrapper (dir : Directory) : EntryWrapper :=
  EntryWrapper.directory dir.name dir.entries

instance : ToString Directory where
  toString d := s!"{d.name} ({d.getSize})"

end Directory

end Composite.ClassBase
