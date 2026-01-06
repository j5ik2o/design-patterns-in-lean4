/-
  Strategy Pattern Implementation in Lean4
  Based on: https://github.com/j5ik2o/design-patterns-in-rust/blob/main/src/strategy.rs

  This implements a Rock-Paper-Scissors game using the Strategy pattern.
-/

namespace Strategy

/-- じゃんけんの手を表す型 -/
inductive Hand where
  | guu : Hand  -- グー (Rock)
  | cho : Hand  -- チョキ (Scissors)
  | paa : Hand  -- パー (Paper)
  deriving Repr, BEq, DecidableEq

namespace Hand

/-- 手の日本語名を取得 -/
def name : Hand → String
  | guu => "グー"
  | cho => "チョキ"
  | paa => "パー"

/-- 手の数値表現 -/
def handValue : Hand → Nat
  | guu => 0
  | cho => 1
  | paa => 2

/-- 数値から手を取得 -/
def getHand : Nat → Hand
  | 0 => guu
  | 1 => cho
  | 2 => paa
  | _ => guu  -- デフォルト

/-- 勝敗判定: 1 = 勝ち, 0 = 引き分け, -1 = 負け -/
def fight (self other : Hand) : Int :=
  if self == other then 0
  else if (self.handValue + 1) % 3 == other.handValue then 1
  else -1

/-- 相手より強いか判定 -/
def isStrongerThan (self other : Hand) : Bool :=
  self.fight other == 1

/-- 相手より弱いか判定 -/
def isWeakerThan (self other : Hand) : Bool :=
  self.fight other == -1

instance : ToString Hand where
  toString := Hand.name

end Hand

/-- 戦略の状態を表す型 -/
structure StrategyState where
  won : Bool := false
  prevHand : Hand := Hand.guu
  -- ProbeStrategy用の履歴マトリックス (3x3)
  history : Array (Array Nat) := #[#[0, 0, 0], #[0, 0, 0], #[0, 0, 0]]
  currentHandValue : Nat := 0
  deriving Repr

/-- 戦略を表す型クラス (Strategy trait に相当) -/
class Strategy (α : Type) where
  /-- 次の手を決定する -/
  nextHand : α → StrategyState → (Hand × StrategyState)
  /-- 勝敗に基づいて学習する -/
  study : α → Bool → StrategyState → StrategyState

/-- WinningStrategy: 勝ったら同じ手を出し続ける戦略 -/
structure WinningStrategy where
  seed : Nat := 0
  deriving Repr

namespace WinningStrategy

/-- 簡易的な擬似乱数生成 (Linear Congruential Generator) -/
def nextRandom (seed : Nat) : Nat × Nat :=
  let newSeed := (seed * 1103515245 + 12345) % (2^31)
  (newSeed % 3, newSeed)

instance : Strategy WinningStrategy where
  nextHand ws state :=
    if state.won then
      (state.prevHand, state)
    else
      let (randVal, _) := nextRandom (ws.seed + state.prevHand.handValue)
      let hand := Hand.getHand randVal
      (hand, { state with prevHand := hand })

  study _ win state :=
    { state with won := win }

end WinningStrategy

/-- ProbeStrategy: 履歴に基づいて確率的に手を選ぶ戦略 -/
structure ProbeStrategy where
  seed : Nat := 0
  deriving Repr

namespace ProbeStrategy

/-- 配列から要素を安全に取得 -/
def arrayGetD (arr : Array α) (i : Nat) (default : α) : α :=
  if h : i < arr.size then arr[i] else default

/-- 履歴マトリックスから合計を計算 -/
def getSum (history : Array (Array Nat)) (hv : Nat) : Nat :=
  if h : hv < history.size then
    let row := history[hv]
    row.foldl (· + ·) 0
  else 0

/-- 履歴に基づいて手を選択 -/
def selectHand (history : Array (Array Nat)) (hv : Nat) (seed : Nat) : Hand :=
  let sum := getSum history hv
  if sum == 0 then
    Hand.getHand (seed % 3)
  else
    let bet := seed % sum
    if h : hv < history.size then
      let row := history[hv]
      let v0 := arrayGetD row 0 0
      let v1 := arrayGetD row 1 0
      if bet < v0 then Hand.guu
      else if bet < v0 + v1 then Hand.cho
      else Hand.paa
    else Hand.guu

/-- 配列の要素を安全に設定 -/
def arraySetD (arr : Array α) (i : Nat) (val : α) : Array α :=
  if h : i < arr.size then arr.set i val h else arr

/-- 履歴マトリックスを更新 -/
def updateHistory (history : Array (Array Nat)) (prevHv nextHv : Nat) : Array (Array Nat) :=
  if h : prevHv < history.size then
    let row := history[prevHv]
    let newVal := arrayGetD row nextHv 0 + 1
    let newRow := arraySetD row nextHv newVal
    history.set prevHv newRow h
  else history

instance : Strategy ProbeStrategy where
  nextHand ps state :=
    let hand := selectHand state.history state.currentHandValue ps.seed
    (hand, { state with prevHand := hand, currentHandValue := hand.handValue })

  study _ win state :=
    if win then
      { state with
        won := true
        history := updateHistory state.history state.prevHand.handValue state.currentHandValue }
    else
      let nextHv := (state.currentHandValue + 1) % 3
      { state with
        won := false
        history := updateHistory state.history state.prevHand.handValue nextHv }

end ProbeStrategy

/-- プレイヤーを表す構造体 -/
structure Player (σ : Type) [Strategy σ] where
  name : String
  strategy : σ
  state : StrategyState := {}
  winCount : Nat := 0
  loseCount : Nat := 0
  gameCount : Nat := 0
  deriving Repr

namespace Player

variable {σ : Type} [Strategy σ]

/-- 次の手を取得 -/
def nextHand (player : Player σ) : Hand × Player σ :=
  let (hand, newState) := Strategy.nextHand player.strategy player.state
  (hand, { player with state := newState })

/-- 勝利時の処理 -/
def win (player : Player σ) : Player σ :=
  let newState := Strategy.study player.strategy true player.state
  { player with
    state := newState
    winCount := player.winCount + 1
    gameCount := player.gameCount + 1 }

/-- 敗北時の処理 -/
def lose (player : Player σ) : Player σ :=
  let newState := Strategy.study player.strategy false player.state
  { player with
    state := newState
    loseCount := player.loseCount + 1
    gameCount := player.gameCount + 1 }

/-- 引き分け時の処理 -/
def draw (player : Player σ) : Player σ :=
  { player with gameCount := player.gameCount + 1 }

/-- 勝率を計算 -/
def winRate (player : Player σ) : Float :=
  if player.gameCount == 0 then 0.0
  else player.winCount.toFloat / player.gameCount.toFloat * 100.0

/-- プレイヤーの状態を文字列で表示 -/
def status (player : Player σ) : String :=
  s!"{player.name}: {player.gameCount}戦 {player.winCount}勝 {player.loseCount}敗"

end Player

/-- ゲームの1ラウンドを実行 -/
def playRound {σ₁ σ₂ : Type} [Strategy σ₁] [Strategy σ₂]
    (p1 : Player σ₁) (p2 : Player σ₂) : Player σ₁ × Player σ₂ :=
  let (hand1, p1') := p1.nextHand
  let (hand2, p2') := p2.nextHand
  let result := hand1.fight hand2
  if result == 1 then
    (p1'.win, p2'.lose)
  else if result == -1 then
    (p1'.lose, p2'.win)
  else
    (p1'.draw, p2'.draw)

/-- 複数ラウンドのゲームを実行 -/
def playGame {σ₁ σ₂ : Type} [Strategy σ₁] [Strategy σ₂]
    (p1 : Player σ₁) (p2 : Player σ₂) (rounds : Nat) : Player σ₁ × Player σ₂ :=
  match rounds with
  | 0 => (p1, p2)
  | n + 1 =>
    let (p1', p2') := playRound p1 p2
    playGame p1' p2' n

end Strategy

-- デモ用の実行例
def demo : IO Unit := do
  -- WinningStrategy vs ProbeStrategy
  let player1 : Strategy.Player Strategy.WinningStrategy := {
    name := "太郎"
    strategy := { seed := 42 }
    state := { prevHand := Strategy.Hand.guu }
  }
  let player2 : Strategy.Player Strategy.ProbeStrategy := {
    name := "花子"
    strategy := { seed := 7919 }
    state := { prevHand := Strategy.Hand.cho }
  }

  let (finalP1, finalP2) := Strategy.playGame player1 player2 100

  IO.println "=== じゃんけんゲーム結果 ==="
  IO.println finalP1.status
  IO.println finalP2.status
  IO.println ""
  IO.println "--- Hand の勝敗判定デモ ---"
  IO.println s!"グー vs チョキ: {if Strategy.Hand.guu.isStrongerThan Strategy.Hand.cho then "グーの勝ち" else "グーの負け"}"
  IO.println s!"チョキ vs パー: {if Strategy.Hand.cho.isStrongerThan Strategy.Hand.paa then "チョキの勝ち" else "チョキの負け"}"
  IO.println s!"パー vs グー: {if Strategy.Hand.paa.isStrongerThan Strategy.Hand.guu then "パーの勝ち" else "パーの負け"}"
