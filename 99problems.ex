defmodule Problems do

  # Types

  @type node_t :: {:one, any()} | {:many, [node_t()]}
  @type rle :: {:one, any()} | {:many, {integer(), any()}}

  # Solutions

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



end
