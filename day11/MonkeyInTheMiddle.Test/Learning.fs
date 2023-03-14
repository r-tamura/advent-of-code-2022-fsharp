module Learning

open Xunit

[<Trait("Category", "Learning")>]
module Regex =
    open System.Text.RegularExpressions

    [<Fact>]
    let ``正規表現でキャプチャされた値はGroupsの2番目以降に保存される`` () =
        let m = Regex.Match("divisible by 23", @"divisible by (\d+)")

        Assert.True(m.Success)
        Assert.Equal("23", m.Groups[1].Captures[0].Value)

[<Trait("Category", "Learning")>]
module Array =
    [<Fact>]
    let ``splitAtで指定されたインデックスの要素は分割後の配列に含まれる`` () =
        Assert.Equal(([| 1; 2 |], [| 3; 4 |]), Array.splitAt 2 [| 1; 2; 3; 4 |])
