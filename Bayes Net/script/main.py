from pgmpy.models import BayesianModel as BM
from pgmpy.factors.discrete import TabularCPD as TC
from pgmpy.inference import VariableElimination as VE


def bayes_net1():
    print('\nquestion 1:\n')
    bayn = BM([('diff', 'grad'), ('intel', 'grad'), ('intel', 'sat'), ('grad', 'lett')])
    cpd_diff = TC(variable='diff', variable_card=2, values=[[0.6], [0.4]])
    cpd_intel = TC(variable='intel', variable_card=2, values=[[0.7], [0.3]])
    cpd_sat = TC(variable='sat', variable_card=2, values=[[0.95, 0.2], [0.05, 0.8]],
                 evidence=['intel'], evidence_card=[2])
    cpd_grad = TC(variable='grad', variable_card=3, values=[[0.3, 0.05, 0.9, 0.5],
                                                            [0.4, 0.25, 0.08, 0.3], [0.3, 0.7, 0.02, 0.2]],
                  evidence=['intel', 'diff'], evidence_card=[2, 2])
    cpd_lett = TC(variable='lett', variable_card=2, values=[[0.1, 0.4, 0.99],
                                                            [0.9, 0.6, 0.01]], evidence=['grad'], evidence_card=[3])
    bayn.add_cpds(cpd_diff, cpd_intel, cpd_sat, cpd_grad, cpd_lett)
    print('\nGraphical Model built!')

    #summary of the network
    print('\nquestion 2:\n')
    print(bayn.nodes())
    print(bayn.edges())
    print(bayn.check_model())

    #finding marginal distribution
    print('\nquestion 3:\n')
    bayn_infer = VE(bayn)
    q_diff = bayn_infer.query(variables=['diff'])
    q_intel = bayn_infer.query(variables=['intel'])
    q_sat = bayn_infer.query(variables=['sat'])
    q_grad = bayn_infer.query(variables=['grad'])
    q_lett = bayn_infer.query(variables=['lett'])
    print('\nMarginal Probabilities computed!')
    print(q_diff)
    print(q_intel)
    print(q_sat)
    print(q_grad)
    print(q_lett)


    #print all conditional independencies
    print('\nquestion 4:\n')
    print(bayn.get_independencies())

    # markov blanket
    print('\nMarkov Blanket\n')
    print(bayn.get_markov_blanket('diff'))
    print(bayn.get_markov_blanket('intel'))
    print(bayn.get_markov_blanket('sat'))
    print(bayn.get_markov_blanket('grad'))
    print(bayn.get_markov_blanket('lett'))


def bayes_net2():
    print('\nquestion 5:\n')
    bayn = BM([('smokes', 'lung_disease'), ('lung_disease', 'shortness_of_breath'), ('lung_disease', 'chest_pain'), ('lung_disease', 'cough'), ('cold', 'cough'), ('cold', 'fever')])
    cpd_smokes = TC(variable='smokes', variable_card=2, values=[[0.2], [0.8]])
    cpd_cold = TC(variable='cold', variable_card=2, values=[[0.02], [0.98]])
    cpd_lung_disease = TC(variable='lung_disease', variable_card=2, values=[[0.1009, 0.001], [0.8991, 0.999]],
                 evidence=['smokes'], evidence_card=[2])
    cpd_shortness_of_breath = TC(variable='shortness_of_breath', variable_card=2, values=[[0.208, 0.01],
                                                            [0.792, 0.99]],
                  evidence=['lung_disease'], evidence_card=[2])
    cpd_chest_pain = TC(variable='chest_pain', variable_card=2, values=[[0.208, 0.01],[0.792, 0.99]],
                                 evidence=['lung_disease'], evidence_card=[2])
    cpd_cough = TC(variable='cough', variable_card=2, values=[[0.7525, 0.505, 0.505, 0.01], [0.2475, 0.495, 0.495, 0.99]],
                  evidence=['lung_disease', 'cold'], evidence_card=[2, 2])
    cpd_fever = TC(variable='fever', variable_card=2, values=[[0.307, 0.01], [0.693, 0.99]],
                          evidence=['cold'], evidence_card=[2])
    bayn.add_cpds(cpd_smokes, cpd_cold, cpd_lung_disease, cpd_shortness_of_breath, cpd_chest_pain, cpd_cough, cpd_fever)
    print('\nGraphical Model built!')

    #summary of the network
    print('\nquestion 6:\n')
    print(bayn.nodes())
    print(bayn.edges())
    print(bayn.check_model())

    #finding marginal distribution
    print('\nquestion 7:\n')
    bayn_infer = VE(bayn)
    q_smokes = bayn_infer.query(variables=['smokes'])
    q_cold = bayn_infer.query(variables=['cold'])
    q_lung_disease = bayn_infer.query(variables=['lung_disease'])
    q_shortness_of_breath = bayn_infer.query(variables=['shortness_of_breath'])
    q_chest_pain = bayn_infer.query(variables=['chest_pain'])
    q_cough = bayn_infer.query(variables=['cough'])
    q_fever = bayn_infer.query(variables=['fever'])
    print('\nMarginal Probabilities computed!')
    print(q_smokes)
    print(q_cold)
    print(q_lung_disease)
    print(q_shortness_of_breath)
    print(q_chest_pain)
    print(q_cough)
    print(q_fever)

    #print all conditional independencies
    print('\nquestion 8:\n')
    print(bayn.get_independencies())

    # markov blanket
    print('\nMarkov Blanket\n')
    print(bayn.get_markov_blanket('smokes'))
    print(bayn.get_markov_blanket('cold'))
    print(bayn.get_markov_blanket('lung_disease'))
    print(bayn.get_markov_blanket('shortness_of_breath'))
    print(bayn.get_markov_blanket('chest_pain'))
    print(bayn.get_markov_blanket('cough'))
    print(bayn.get_markov_blanket('fever'))





if __name__ == '__main__':
    #question1-4
    print('\nquestion 1-4')
    bayes_net1()
    #question 5-8
    print('\nquestion 5-8')
    bayes_net2()


