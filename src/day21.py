# start at 21:21
# first star at 21:40
# Doing live, explaining to a friend, in python
# input is
# Player 1 starting position: 2
# Player 2 starting position: 7
import functools
from dataclasses import dataclass

playersInit = (2, 7)

@dataclass(frozen=True)
class Player:
    position:int = 0
    score:int = 0

def die():
    while True:
        for i in range(1, 101):
            yield i

def calculer_nouveau_score(player, valeur_de):
    new_pos = (((player.position + valeur_de) - 1) % 10) + 1
    return Player(position=new_pos,
                  score = player.score + new_pos)

def play(player, d):
    dpos = sum(next(d) for _ in range(3))
    return calculer_nouveau_score(player, dpos)

def init_player(name, start):
    return {'position': start, 'score': 0, 'name': name}

def day(p0, p1):
  players = [p0, p1]
  
  d = die()
 
  nbDiceRoll = 0
  while players[1].score < 1000:
      p0_bis = play(players[0], d)
      players = [players[1], p0_bis]
      nbDiceRoll += 3
  
  print(players)
  print(nbDiceRoll)

  return nbDiceRoll * players[0].score

#print(day(Player(position=4), Player(position=8)))
#print(day(Player(position=2), Player(position=7)))


# c((score0, pos0), (score1, pos1)) -> (v0, v1)
# si score1 > 1000 -> (0, 1)
# sinon:
#   flip c((score1, pos1), (score2 + pos2bis, pos2bis = pos2 + 1))
#   + c((score1, pos1), (score2 + pos2bis, pos2bis = pos2 + 2))
#   + c((score1, pos1), (score2 + pos2bis, pos2bis = pos2 + 3))

@functools.lru_cache(maxsize=None)
def game(p1, p2):
    if p2.score >= 21:
        return (0, 1)
    else:
        result = (0, 0)
        for dice_outcome in dice_outcomes:
          # On fait jouer le joueur 1 dans un univers
          p1_bis = calculer_nouveau_score(p1, dice_outcome)

          # On continue la partie dans les 3 univers
          result_bis = game(p2, p1_bis)[::-1]

          result = sum_tuple(result, result_bis)
        return result

def sum_tuple(t0, t1):
    return (t0[0] + t1[0], t0[1] + t1[1])

def star2(p0, p1):
  return game(p0, p1)

def dice_outcomes_():
    for i in range(1, 4):
        for j in range(1, 4):
            for k in range(1, 4):
                yield i + j + k

dice_outcomes = list(dice_outcomes_())

#print(star2(Player(position=4), Player(position=8)))
print(max(star2(Player(position=2), Player(position=7))))
