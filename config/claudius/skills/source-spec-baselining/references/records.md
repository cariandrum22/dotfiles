# 記録の設計

既存の台帳/schemaを優先する。以下は必要情報のモデルであり、全ファイル作成や名称変更の要求ではない。小さい案件では一つのJSON/Markdownにまとめてよい。

| 記録 | 主な内容 |
| --- | --- |
| Scope / completion condition | 原依頼、逐語の終了条件、判定scope、対象外、既存権限 |
| Sources / acquisition | ID、版、authority根拠、URL、日時、raw/derived hash、変換来歴 |
| Units / reading | 区画span、read/unread/inherited、分類、理由、文脈対応 |
| Judgments | stable ID、actor/action/strength/trigger、anchor、理由、composite、適用状態 |
| Dependencies / errata | 取り込み範囲/版、公式情報、ローカル判断、理由、queue状態と終了条件 |
| Mapping / changes | ID移行、field差分、作業/証拠型への対応、旧証拠再利用の別判断 |
| Validation | command、tool/version、入出力hash、fresh/replay、門の射程、対照の期待/観測/限界 |
| Review receipt | 実レビューのpath/digest、verdict、scope、所見、条件付き結論 |
| Adoption / state | 採用者、根拠、正確な採用byte集合、状態before/after、閉じる命題と残件 |

## Anchorとhash

解決可能なsource ID/path、source file SHA-256、byte offset/length、引用byteのSHA-256を持つ。引用textとLF行は読みやすさの補助。raw/derivedを区別する。

実際の原典byteから値を計算する。PDFページ/印字ページ、HTMLのstrike/underline等は必要な場合に追加する。JSONを別rootへコピーしてもanchorのpathを黙って書き換えず、coordinate rootを明記する。

whole-file hashはbyte同一性に使う。recordのcanonical hashはUTF-8/key順/空白等の正規化規則も宣言する。canonical同一をbyte同一と呼ばない。

## 語彙と理由

機械が解釈するdisposition、queue state、decision、roleは可能な範囲で閉じた語彙を宣言する。公式ラベルと作成者ラベルは別にする。改名する場合は旧値・新値・意味の差を保存する。

自由文reasonを残し、重要な判断の空欄を許さない。ただし非空や有効enumであることは正しい意味判断を保証しない。有効値間のすり替えが通る検査の限界も示す。

## 状態は複数の軸で持つ

「封緘→受理→全完了」という単一状態に畳まない。

- immutable packet: candidate段階でも封緘できる。reviewは外側へ置き、候補byteは変更しない。
- decision: 未審査、限定受理、修正必要等。語彙は案件で定義する。
- adoption: 受理された候補を実際にbaselineへ採用したか。
- completion: どの元条件をどのscopeで満たしたか。依存閉鎖・実装適合・公開許可等は別判定。

原文忠実性はaccepted、packetはsealed、採用は未実施、実装適合はnot-assessed、という状態は整合する。採用条件付きで工程終了が認められ、許可済みの採用を行った後は当該工程を完了に更新する。情報所見が残るだけで完了を否定しない。

未調査をfalse/不適合と混同しない。既存schemaがブール値なら、何の達成を表すfalseか明記する。保証の総数や工程名は対象案件から取り、過去事例の定数を転記しない。

## 受領と履歴

レビュー要約だけで実記録を受領したことにしない。実ファイルに到達できなければ `reported-but-not-verified` 等として不足を記録する。hash再計算済みの受領と報告値だけの保存を区別する。

採用時に旧candidateのpendingを変更する必要はない。新STATEに採用digestと適用範囲を結び、どの状態を更新するか宣言する。旧証拠の判定や情報所見は、候補採用だけでは移行/閉鎖しない。

hashを循環参照させない。例えば、実review → receipt → adoption → state → ledger appendの順に束縛し、最後に外側manifestと外部受領記録で全体を束縛する。

## 小さな構成例

```text
packet/
  SCOPE.json
  inputs/                 # 必要な原典/旧記録の選択コピー
  records/                # 判断、依存、対応と差分
  scripts/                # 必要な場合だけ。作成者専用/読取専用を区別
  out/                    # 封緘前の結果
  REVIEW_REQUEST.md
  SHA256SUMS
final-check/              # packet外の最終照合
review-root/              # reviewer自身の別root
adoption-root/            # 受領後の採用/閉鎖記録
```

説明用の構成でありrootを増やすことが目的ではない。既存manifestの配置に合わせて縮める。reviewer私有データや歴史的パケット全体の再帰コピーを標準にしない。
