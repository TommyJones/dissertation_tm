# dissertation_tm
Code and writing for my topic model-based dissertation


# Outline

1. **Formal (mathematical) definition of a topic model**
   Types/taxonomy of topic models
   Justification for this definition
2. **Simulating corpora**
   Conforms to gross statistical properties of human language
   Conforms to functional form of probabilistic topic models (DTM-based)
3. **Evaluation metrics for topic models**
   Born of simulations
   r-squared, prob. coherence, likelihood methods
   show on real corpora
4. **Properties of corpora (real and simulated)**
   Ties back to parameters of simulations
   doc lengths, # of documents, topical heterogeneity
   tests on corpora: # of topics, # languages (structural break), sparsity parameters, stopword distributions
5. **Etc.**
   Interpretation of topic models based on CTMs
   Comparing word2vec, glove, etc on LDA (and other matrix decomp methods) on a CTM
6. **Extensions/posibilities/relevance (Imagine a world...)**
   Real-world applicability
   use in larger analyses etc.
   non-english/mixed language cases
   
# Discrete research questions

Fill in specific questions below in service of the above. I'll make markdown and/or scripts to address them.

## What's the best way to parameterize simulations WRT stopwords?

* Can I parametrize based on independent draws of dirichlet distribution where parameter conforms to power law
* Plots: log log where x-axis is sorted by log(rank) of entire corpus. Individual curves (documents or other subsamples) plotted.
* plot: multiple documents (same corpus) on a chart with stopwords in red
* plot: very different documents (different corpora) on a chart with stopwords in red
* plot: different draws of same dirichlet distribution
* statistic measure of variability between documents/topics with power law distributions
  Purpose is that I need to know how "valid" simulations are compared to real world
  Also, does mixed language case fundamentally alter assumptions/evaluation methods? 
  idea - correlation of log-log for different distributions indexed by term

