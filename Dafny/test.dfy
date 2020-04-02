datatype Nat = Z | S(Nat)

function add(x : Nat, y : Nat): Nat 
decreases x
{
    match x
        case Z => y
        case S(n) => S(add(n, y))
}

ghost method addZ (y : Nat) 
ensures y == add(Z, y)
{}

ghost method addS(x : Nat, y : Nat)
ensures S(add(x, y)) == add(x, S(y)) {}

ghost method add_comm(x : Nat, y : Nat)
ensures add(x, y) == add(y, x) {
    match x
        case Z => calc {
            add(Z, y); == y; == { addZ(y); } add(y, Z);
        }
        case S(x') => calc {
                add(S(x'), y);
            ==  S(add(x', y)); // definition
            ==	{ add_comm(x',y); } // IH
                S(add(y, x'));
            ==	{ addS(y, x'); }
                add(y, S(x'));
        }
}

// function method calculateSum(s : seq<int>): int 
// decreases |s|
// ensures forall xs :: |xs| == 1 ==> calculateSum(xs) == xs[0]
// ensures forall i :: 0 <= i < |s| ==> 
//                     calculateSum(s[..i]) + calculateSum(s[i..]) == calculateSum(s)
// {
//     if |s| == 0 then 0 else s[0] + calculateSum(s[1..])
// }

// method impCalculateSum(arr : array<int>) returns (ans: int) 
// ensures ans == calculateSum(arr[..])
// ensures arr == old(arr)
// {
//     ans := 0;
//     var cur := 0;
//     ghost var gho := calculateSum(arr[..]);
//     if (arr.Length > 0) {
//         while (cur < arr.Length) 
//         decreases arr.Length - cur
//         invariant cur <= arr.Length
//         invariant arr == old(arr)
//         invariant cur < arr.Length ==> ans + calculateSum(old(arr[cur..])) == gho
//         invariant cur < arr.Length ==> ans == calculateSum(old(arr[..cur]))
//         {
//             ans := ans + arr[cur];
//             assert ans == calculateSum(arr[..cur]);
//             cur := cur + 1;
//         }
//     }
// }

datatype Maybe<T> = Nothing | Just(T)

// function method find<T(==)>(s : seq<T>): Maybe<T> {

// }

// method testSum() {
//     assert calculateSum([1, 2, 3]) == 6;
// }

function method max(x : nat, y : nat): nat 
ensures x <= max(x, y) && y <= max(x, y)
ensures x == max(x, y) || y == max(x, y)
{
    if x >= y then x else y
}

function method maxOf(s : seq<nat>): nat
decreases |s| 
ensures forall i :: 0 <= i < |s| ==> s[i] <= maxOf(s)
{
    if |s| == 0 then 0 else max(s[0], maxOf(s[1..]))
}

method testMaxOf() {
    assert maxOf([1, 4, 5, 2, 3]) == 5;
    assert maxOf([]) == 0;
    assert maxOf([249]) == 249;
}

method Find<T(==)>(arr: array<T>, x: T) returns (i: int)
ensures i >= 0
ensures i < arr.Length ==> arr[i] == x
{
    i := 0;
    while (i < arr.Length && arr[i] != x) 
    invariant i <= arr.Length
    invariant forall p :: 0 <= p < i ==> arr[p] != x
    {
        i := i + 1;
    }
    return i;
}

method indexOf<T(==)>(arr : array<T>, x : T) returns (i : int)
ensures i >= -1
ensures 0 < i < arr.Length ==> arr[i] == x
{
    i := 0;
    while (i < arr.Length && arr[i] != x) 
    decreases arr.Length - i
    invariant i <= arr.Length
    invariant forall j | 0 <= j < i :: arr[j] != x
    {
        i := i + 1;
    }
    if (i == arr.Length) {
        return -1;
    } else {
        return i;
    }
}

function method mapSeq(s : seq<int>, f : (int -> int)): seq<int>
decreases |s|
requires forall x, y : int :: x == y ==> f(x) == f(y)
ensures |s| == |mapSeq(s, f)|
ensures forall i :: 0 <= i < |s| ==> f(s[i]) == mapSeq(s, f)[i]
{
    if |s| == 0 then [] else [f(s[0])] + mapSeq(s[1..], f)
}

// method impMap(arr : array<int>, f : (int -> int)) 
// modifies arr
// requires forall x, y : int :: x == y ==> f(x) == f(y)
// ensures forall i :: 0 <= i < arr.Length ==> arr[i] == mapSeq(old(arr[..]), f)[i]
// {
//     var cur := 0;
//     ghost var ans := mapSeq(old(arr[..]), f);
//     if (arr.Length > 0) {
//         while (cur < arr.Length)
//         decreases arr.Length - cur
//         invariant cur <= arr.Length
//         invariant arr.Length == |ans|
//         invariant arr.Length == old(arr.Length)
//         invariant forall i :: cur < i < arr.Length ==> f(old(arr[i])) == ans[i]
//         invariant forall i :: cur < i < arr.Length ==> old(arr[i]) == arr[i]
//         invariant cur + 1 < arr.Length ==> arr[cur + 1] == old(arr[cur + 1])
//         invariant forall i :: 0 <= i < cur ==> arr[i] == ans[i]
//         {
//             var tmp := arr[cur];
//             arr[cur] := f(tmp);
//             assert f(tmp) == arr[cur];
//             assert tmp == old(arr[cur]);
//             assert cur < arr.Length;
//             assert f(old(arr[cur])) == ans[cur];
//             assert arr[cur] == f(old(arr[cur]));
//             assert arr[cur] == ans[cur];
//             cur := cur + 1;
//         }
//     }
// }