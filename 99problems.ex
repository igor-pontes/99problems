defmodule Problems do

  # Types

  @type node_t :: {:one, any()} | {:many, [node_t()]}
  @type rle :: {:one, any()} | {:many, {integer(), any()}}

  # Solutions

  # Lists

  @spec last(list()) :: any()
  def last(l) do
   case l do
    [] -> nil
    [head | []] -> head
    [_ | tail] -> last(tail)
   end
  end

  @spec last_two(list()) :: list()
  def last_two(lst) do
   case lst do
    [] -> nil
    [x | tail] -> case tail do
        [] -> nil
        [y | []] -> [x, y]
        _ -> last_two(tail)
      end
    end
  end

  @spec at(integer(), list()) :: any()
  def at(nth, lst) do
    case lst do
      [] -> if nth > 0 do raise "Index out of bounds" else nil end
      [head | tail] -> if nth == 1 do head else at(nth-1, tail) end
    end
  end

  @spec len(list()) :: integer()
  def len(lst) do
    case lst do
      [] -> 0
      [_ | tail] -> 1 + len(tail)
    end
  end

  @spec rev(list()) :: list()
  def rev(lst) do
    case lst do
      [] -> []
      [head | tail] -> rev(tail) ++ [head]
    end
  end

  @spec is_palindrome(charlist()) :: boolean()
  def is_palindrome(lst) do
    lst == rev(lst)
  end

  @spec flatten([node_t()]) :: list()
  def flatten(lst) do
    case lst do
      [] -> []
      [head | tail] -> case head do
        {:one, x} -> [x] ++ flatten(tail)
        {:many, lst} -> flatten(lst) ++ flatten(tail)
      end
    end
  end

  @spec compress(list()) :: list()
  def compress(lst) do
    case lst do
      [] -> []
      [x | tail] -> case tail do
        [] -> [x]
        [y | _] -> if x != y do [x] ++ compress(tail) else compress(tail) end
      end
    end
  end

  @spec pack(list()) :: list()
  def pack(lst) do
    aux = fn (c, lst, aux) ->
      case {c, lst} do
        {[], []} -> []
        {c, []} -> c
        {[], [h | t]} -> aux.([[h]], t, aux)
        {[hc | tc], [h | t]} -> if List.first(hc) == h do
          aux.([[h | hc] | tc], t, aux)
          else aux.([[h] | [hc | tc]], t, aux)
        end
      end
    end
    Problems.rev(aux.([], lst, aux))
  end

  @spec encode(list()) :: list({integer(), any()})
  def encode(lst) do
    aux = fn (c, lst, aux) ->
      case {c, lst} do
        {[], []} -> []
        {c, []} -> c
        {[], [h | t]} -> aux.([{1, h}], t, aux)
        {[{n, ch} | tc], [h | t]} -> if ch == h do
          aux.([{n+1, ch} | tc], t, aux)
          else aux.([{1, h} | [{n, ch} | tc]], t, aux)
        end
      end
    end
    Problems.rev(aux.([], lst, aux))
  end

  @spec m_encode(list()) :: [rle()]
  def m_encode(lst) do
    # Worse performance but redable code
    Problems.encode(lst)
    |> Enum.map(fn {n, c} ->
      if n == 1 do {:one, c} else {:many, {n, c}} end
    end)
  end

  @spec m_encode2(list()) :: [rle()]
  def m_encode2(lst) do
    counter = fn (count, ch) ->
      if count == 0 do {:one, ch} else {:many, {count + 1, ch}} end
    end
    aux = fn (clst, count, lst, aux, counter) ->
      case lst do
        [] -> []
        [x] -> [counter.(count, x) | clst]
        [x | t] -> if x == List.first(t) do
          aux.(clst, count + 1, t, aux, counter)
          else aux.([counter.(count, x) | clst], 0, t, aux, counter)
        end
      end
    end
    Problems.rev(aux.([], 0, lst, aux, counter))
  end

  @spec decode([rle()]) :: list()
  def decode(lst) do
    aux = fn (c, lst, aux) ->
      case lst do
        [] -> c
        [{:one, ch} | tail] -> aux.([ch | c], tail, aux)
        [{:many, {n, ch}} | tail] -> if n == 2 do
          aux.([ch | c], [{:one, ch} | tail], aux)
          else aux.([ch | c], [{:many, {n-1, ch}} | tail], aux)
        end
      end
    end
    Problems.rev(aux.([], lst, aux))
  end

  @spec duplicate(list()) :: list()
  def duplicate(lst) do
    case lst do
      [] -> []
      [h | t] -> [h, h] ++ duplicate(t)
    end
  end

  # can do better
  @spec replicate(list(), integer()) :: list()
  def replicate(lst, n) do
    aux = fn (ch, n, aux) ->
      if n == 0 do [] else [ch] ++ aux.(ch, n-1, aux) end
    end
    case lst do
      [] -> []
      [h | t] -> aux.(h, n, aux) ++ replicate(t, n)
    end
  end

  @spec drop(list(), integer()) :: list()
  def drop(lst, n) do
    aux = fn (lst, c, aux) -> case lst do
        [] -> []
        [h | t] -> if c == 1 do t else [h] ++ aux.(t, c-1, aux) end
      end
    end
    aux.(lst, n, aux)
  end

  @spec split(list(), integer()) :: {list(), list()}
  def split(lst, n) do
    aux = fn (acc, lst, n, aux) -> case lst do
        [] -> { Problems.rev(acc), [] }
        [h | t] -> if n >= 2 do aux.([h | acc], t, n-1, aux) else { Problems.rev([h | acc]), t }
        end
      end
    end
    aux.([], lst, n, aux)
  end

  @spec slice(list(), integer(), integer()) :: list()
  def slice(lst, i, k) do
    case lst do
      [] -> []
      [h | t] -> if i == 0 and k >= 0 do [h | slice(t, 0, k-1)] else if k < 0 do [] else slice(t, i-1, k-1) end
      end
    end
  end

  @spec rotate(list(), integer()) :: list()
  def rotate(lst, n) do
    aux = fn (acc, lst, n, aux) -> case lst do
        [] -> []
        [h | t] -> if n == 1 do t ++ Problems.rev([h | acc]) else aux.([h | acc], t, n-1, aux) end
      end
    end
    aux.([], lst, n, aux)
  end

  @spec remove_at(integer(), list()) :: list()
  def remove_at(n, lst) do
    case lst do
      [] -> []
      [h | t] -> if n == 0 do t else [h | remove_at(n-1, t)] end
    end
  end

  @spec insert_at(any(), integer(), list()) :: list()
  def insert_at(s, n, lst) do
    case lst do
      [] -> []
      [h | t] -> if n == 0 do [s | [h | t]] else [h | insert_at(s, n-1, t)] end
    end
  end

  # Not using reverse.
  @spec range(integer(), integer()) :: list()
  def range(i, k) do
    aux = fn (i, f, aux) ->
      if k == i do [i] else [i | aux.(f.(i), f, aux)] end
    end
    if k > i do aux.(i, &(&1+1), aux) else aux.(i, &(&1-1), aux) end
  end

  # how to avoid "++" operator here?
  @spec rand_select(list(), integer()) :: list()
  def rand_select(lst, i) do
    pick = fn ({acc, lst}, temp, k, pick) ->
      case lst do
        [] -> {acc, lst}
        [h | t] -> if k == 0 do {[h | acc], temp ++ t} else pick.({acc, t}, [h | temp], k-1, pick) end
      end
    end
    aux = fn (t, len, i, aux) ->
      if i == 0 do elem(t, 0) else aux.(pick.(t, [], :rand.uniform(len)-1, pick), len-1, i-1, aux) end
    end
    aux.({[], lst}, length(lst), i, aux)
  end

  # Basically just a "rand_select".
  # I copied it to avoid calling "length()", which is not O(1).
  @spec lotto_select(integer(), integer()) :: list()
  def lotto_select(i, k) do
    pick = fn ({acc, lst}, temp, k, pick) ->
      case lst do
        [] -> {acc, lst}
        [h | t] -> if k == 0 do {[h | acc], temp ++ t} else pick.({acc, t}, [h | temp], k-1, pick) end
      end
    end
    aux = fn (t, len, i, aux) ->
      if i == 0 do elem(t, 0) else aux.(pick.(t, [], :rand.uniform(len)-1, pick), len-1, i-1, aux) end
    end
    aux.({[], range(1, k)}, k, i, aux)
  end

  @spec permutation(list()) :: list()
  def permutation(lst) do
    pick = fn ({acc, lst}, temp, k, pick) ->
      case lst do
        [] -> {acc, lst}
        [h | t] -> if k == 0 do {[h | acc], temp ++ t} else pick.({acc, t}, [h | temp], k-1, pick) end
      end
    end
    aux = fn (t, len, aux) ->
      if len == 0 do elem(t, 0) else aux.(pick.(t, [], :rand.uniform(len)-1, pick), len-1, aux) end
    end
    aux.({[], lst}, length(lst), aux)
  end

  # can't believe it is this simple...
  @spec extract(integer(), list()) :: list()
  def extract(i, lst) do
    if i == 0 do [[]] else
      case lst do
        [] -> []
        [h | t] ->
          with_h = Enum.map(extract(i-1, t), fn l -> [h | l] end)
          without_h = extract(i, t)
          with_h ++ without_h
      end
    end
  end

  @spec group(list(), list()) :: list()
  def group(lst, grps) do
    aux = fn (i, lst, aux) ->
      if i == 0 do [{[], lst}] else
        case lst do
          [] -> []
          [h | t] ->
            with_h = Enum.map(aux.(i-1, t, aux), fn {arr, rem} -> {[h | arr], rem} end)
            without_h = Enum.map(aux.(i, t, aux), fn {arr, rem} -> {arr, [h | rem]} end)
            with_h ++ without_h
        end
      end
    end
    iter = fn (acc, grps, iter) ->
      case grps do
        [] -> acc
        [h | t] -> case acc do
          [{lst, []}] -> iter.(Enum.map(aux.(h, lst, aux), fn {arr, rem} -> {[arr], rem} end), t, iter)
          arr -> iter.(Enum.flat_map(Enum.map(acc, fn {arr, rem} ->
              Enum.map(aux.(h, rem, aux), fn {h, t} -> {arr ++ [h], t} end) end), &(&1)),
            t, iter)
        end
      end
    end
    Enum.map(iter.([{lst, []}], grps, iter), fn {arr, _} -> arr end)
  end

  @spec length_sort(list()) :: list()
  def length_sort(lst) do
    fst = fn e -> elem(e, 0) end
    # If "e" greater than h, it could be greater than other elements of "lst"
    ins = fn (e, lst, ins) ->
      case lst do
        [] -> [e]
        [h | t] -> if fst.(e) < fst.(h) do [e | [h | t]] else [h | ins.(e, t, ins)] end
      end
    end
    iter = fn (lst, iter) ->
      case lst do
        [] -> []
        [h | t] -> ins.(h, iter.(t, iter), ins)
      end
    end
    lst = Enum.map(lst, fn e -> {len(e), e} end)
    Enum.map(iter.(lst, iter), fn e -> elem(e, 1) end)
  end

  @spec frequency_sort(list()) :: list()
  def frequency_sort(lst) do
    Enum.group_by(lst, &Kernel.length/1)
    |> Enum.map(fn e -> elem(e,1) end)
    |> Problems.length_sort()
  end

end
