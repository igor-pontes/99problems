defmodule Problems do

  # Types

  @type node_t :: {:one, any()} | {:many, [node_t()]}

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
        [y | tail] -> if x == y do [x] ++ compress(tail) end
      end
    end
  end

  @spec pack(list()) :: list()
  def pack(lst) do
    # TODO
  end
end
