# 原典仕様・候補のレビュー依頼

このファイルはテンプレート。`{{...}}`を具体化し、不要な項目は削除する。件数、digest、PASS、読了の主張を予定値で埋めない。

## 判断してほしいこと

{{具体的な候補/差分と、判断が必要な理由}}

元の終了条件（逐語）: {{condition}}

Requested scope: {{受理を求める範囲}}

今回判定しないもの: {{別工程の未調査/適合/実行/承認}}

## 固定入力

- 対象packetとmanifest: {{path/hash}}
- 原典の版と選択/全文範囲: {{source inventory}}
- 前reviewの実記録と受領scope: {{actual paths/digests, or no prior review}}
- 保存方式: {{選択コピー/完全archive、元path/hash、外部参照の到達性}}
- 候補差分とID移行: {{old/new、変更field、保持部分、影響先}}

## 原文照合の論点

{{actor/action/強度/条件/例外、文脈導入、非action処置、版境界、取り込み/errata、旧文/証拠対応のうち、今回判断すべき具体的事項}}

{{継承範囲と今回のfresh範囲、未読、構造被覆と意味判断の区別}}

## 検証と再実行

{{実際に動作確認済みのコマンド、tool/version、外部の新規出力先}}

{{検査対象field/行、固定期待値の出所、検査器自身の束縛、自由文等の未検査範囲}}

{{対照と再生成の実結果、非再現箇所と理由、CLI/moduleの実行区分}}

原典、packet、旧review、共有inodeへ書かず、reviewerの結果は新しい外部rootへ保存する。author-onlyの取得/台帳/封緘scriptは再実行対象と区別する。

## 結論の分離

関連するものについて、次を別々に述べてください。

1. 候補をどのscopeで受理できるか。
2. 対象課題は解決したか。採用時に閉じられる命題と残る条件は何か。
3. 元の終了条件は満たされるか。採用条件付きなら明記し、当該工程の残条件と別工程の作業を区別する。

必須所見と情報所見を区別してください。任意改善を元条件へ無理由に追加せず、未充足の阻害条件があれば具体的に示してください。

## 実施区分と独立性

fresh execution: {{実行したこと}}

source reading: {{実際に読んだ原文/表示/範囲}}

replay/inherited: {{再読・再実行せず継承した判断}}

not run/unread: {{未実行・未読}}

作成者/過去reviewへの関与・共有host/tool等: {{actual involvement}}

このレビューが与える承認の種類: {{source judgment / engineering review / explicitly authorized human approval等の実際のscope}}
