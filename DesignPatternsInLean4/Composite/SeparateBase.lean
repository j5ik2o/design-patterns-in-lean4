/-
  Composite Pattern - Separate Types Implementation
  ファイルとディレクトリを別の型として明確に区別するアプローチ。
-/

namespace Composite.SeparateBase

/-- ファイルを表す構造体 -/
structure FileEntry where
  name : String
  size : Nat
  deriving Repr, BEq

/-- エントリ（再帰的に定義） -/
inductive Entry where
  | file : FileEntry → Entry
  | dir : String → List Entry → Entry
  deriving Repr

namespace FileEntry

instance : ToString FileEntry where
  toString f := s!"{f.name} ({f.size})"

end FileEntry

namespace Entry

def getName : Entry → String
  | file f => f.name
  | dir name _ => name

def getSize : Entry → Nat
  | file f => f.size
  | dir _ entries => entries.foldl (fun acc e => acc + e.getSize) 0

partial def printWithPrefix (e : Entry) (prefix_ : String) : String :=
  match e with
  | file f => s!"{prefix_}/{f.name} ({f.size})"
  | dir name entries =>
    let header := s!"{prefix_}/{name}"
    let newPrefix := s!"{prefix_}/{name}"
    let children := entries.map (fun child => child.printWithPrefix newPrefix)
    String.intercalate "\n" (header :: children)

def print (e : Entry) : String :=
  printWithPrefix e ""

instance : ToString Entry where
  toString e := s!"{e.getName} ({e.getSize})"

/-- ディレクトリを作成するヘルパー -/
def mkDir (name : String) (entries : List Entry := []) : Entry :=
  Entry.dir name entries

/-- ファイルを作成するヘルパー -/
def mkFile (name : String) (size : Nat) : Entry :=
  Entry.file { name := name, size := size }

end Entry

end Composite.SeparateBase
