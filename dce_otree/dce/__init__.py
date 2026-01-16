from otree.api import *
import random
import json
import os

class C(BaseConstants):
    NAME_IN_URL = 'reco'
    PLAYERS_PER_GROUP = None
    NUM_ROUNDS = 8

    json_path = os.path.join(os.path.dirname(__file__), "design_dce.json")
    with open(json_path, "r", encoding="utf-8") as f:
        DCE_QUESTIONS = json.load(f)

class Subsession(BaseSubsession):
    pass

class Group(BaseGroup):
    pass

class Player(BasePlayer):
    def get_questions(self, block):
        q = [set for set in C.DCE_QUESTIONS if set[0]["block"] == block ]
        return q

    block = models.IntegerField()
    age = models.IntegerField(label='Quel est votre âge?', min=13, max=125)
    
    genre = models.IntegerField(
        choices=[[1, 'Homme'], [0, 'Femme']],
        label='Quel est votre genre?',
        widget=widgets.RadioSelect,
    )

    etudes = models.IntegerField(
        widget=widgets.RadioSelect,
        label="Quel est votre niveau d'études actuel?",
        choices=[[3, 'Licence 3'], [4, 'Master 1'],  [5, 'Master 2']],
    )

    filiere = models.IntegerField(
        widget=widgets.RadioSelect,
        label="Quelle est votre filère principale?",
        choices=[[0, 'Economie'], [1, 'Management'], [2, 'AES']],
    )

    selected = models.IntegerField(
        widget=widgets.RadioSelect,
        label="Vous préfèrez:",
        choices=[[1, 'Choix A'], [2, 'Choix B']],
    )

    set = models.IntegerField()

# PAGES
class Introduction(Page):
    @staticmethod
    def is_displayed(player: Player):
        return player.round_number == 1

class Conclusion(Page):
    @staticmethod
    def is_displayed(player: Player):
        return player.round_number == C.NUM_ROUNDS

class Demographics(Page):
    @staticmethod
    def is_displayed(player: Player):
        if 'block' not in player.participant.vars:
            player.participant.vars['block'] = random.choice([1,2])

        return player.round_number == 1

    form_model = 'player'
    form_fields = ['age', 'genre', 'etudes', 'filiere']

class DCE(Page):
    form_model = 'player'
    form_fields = ['selected']

    @staticmethod
    def vars_for_template(player):
        block = player.participant.vars['block']

        DCE_question = player.get_questions(block)[player.round_number - 1]

        return dict(
            DCE_question=DCE_question,
        )

    @staticmethod
    def before_next_page(player, timeout_happened):
        block = player.participant.vars['block']

        question = player.get_questions(block)[player.round_number - 1][0]

        player.block = question['block']
        player.set = question['set']

page_sequence = [Introduction, Demographics, DCE, Conclusion]
