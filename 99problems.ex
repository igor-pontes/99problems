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
    aux = fn (i, k, f, aux) ->
      if k == i do [i] else [i | aux.(f.(i), k, f, aux)] end
    end
    if k > i do aux.(i, k, &(&1+1), aux) else aux.(i, k, &(&1-1), aux) end
  end

  @spec rand_select(list(), integer()) :: list()
  def rand_select(i, k) do
    #TODO
  end

  @spec lotto_select(integer(), integer()) :: list()
  def lotto_select(i, k) do
    #TODO
  end

  @spec permutation(list()) :: list()
  def permuation(lst) do
    #TODO
  end

  @spec extract(integer(), list()) :: list()
  def extract(lst) do
    #TODO
  end

  @spec group(list(), list()) :: list()
  def group(lst) do
    #TODO
  end

  @spec length_sort(list()) :: list()
  def length_sort(lst) do
    #TODO
  end

  @spec frequency_sort(list()) :: list()
  def frequency_sort(lst) do
    #TODO
  end

end
