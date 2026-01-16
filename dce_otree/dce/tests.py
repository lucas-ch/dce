from . import *
import random

class PlayerBot(Bot):
    def play_round(self):
        # réponses par type de répodants: r1 préfèrent le télétravail, r2 l'évitent
        probs_r1 = [0.7, 0.4, 0.6, 0.8, 0.0, 0.0, 0.7, 1.0, 0.5, 0.7, 0.8, 0.2, 0.1, 0.0, 0.2, 1.0]
        probs_r2 = [0.0, 1.0, 1.0, 0.0, 0.4, 0.2, 1.0, 0.2, 0.0, 0.2, 1.0, 0.8, 0.6, 0.5, 0.0, 0.2]

        if self.round_number == 1:
            # simulation du type de répondant, filière et études
            self.participant.vars['respondent_type'] = random.choices(['r1', 'r2'], weights=[70, 30])[0]       
            filiere_sim = random.choices([0, 1, 2], weights=[80, 10, 10])[0]            
            etudes_sim = random.choices([5, 3, 4], weights=[80, 10, 10])[0]

            yield Introduction

            yield Demographics, dict(
                age=random.randint(21, 26),
                genre=random.choice([1, 0]),
                etudes=etudes_sim,
                filiere=filiere_sim,
            )

        # question auquelle le participant est en train de répondre
        block = self.player.participant.vars['block']
        question_data = self.player.get_questions(block)[self.player.round_number - 1]        
        set_index = question_data[0]["set"] - 1
        
        # choix de la réponse à la question en fonction du type de répondant
        resp_type = self.participant.vars['respondent_type']        
        p_choice_2 = probs_r1[set_index] if resp_type == 'r1' else probs_r2[set_index]
        if random.random() < p_choice_2:
            choice_index = 2
        else:
            choice_index = 1

        yield DCE, dict(selected=choice_index)