In this project, we have accessed the 40,000 physician notes created medical aspirants. These notes, intended for ten standardized patients, form a distinctive dataset for examination. As the patients remain consistent across all note writers, the notes naturally cluster into ten groups. This project serves as an excellent illustration of unsupervised learning, wherein the known grouping of patients can be employed for subsequent analysis.

Major Packages used: sklearn, nltk, umap, pandas

This project involves the following major steps:
1. Data Preprocessing:
    We have first converted the text to a consistent case format and remove punctuation and special characters. 
    Addressed typos and spelling errors using automated correction libraries - Speller. 
    Standardizing formats for dates, numbers, and currencies ensures uniformity across the dataset. 
    Handled contractions by expanding them, such as converting "can't" to "cannot," to maintain clarity and consistency. 
    Finally, Applied a stop word list helps filter out unnecessary words, streamlining the analysis process.
2. Created a Document-Term Matrix (DTM) using TF-IDF
3. Machine Learning Implementation: Performed Normalization of the DTM
4. Applied Isolation Forest for outlier analysis
5. Performed Dimensionality Reduction using UMAP with 2 components, 0.5 min_dist and 40 neighbors with cosine distance metric
6. Then applied the Unsupervised ML Algorithm: Gaussian Mixture Model (GMM), SVD and K-Means and analysed them.
We have used cross_val_score, BIC score and silhouette score to determine the number of clusters and visualized it as shown in  the graph.

Results and conclusion:
1) Using SVD and K-Means Clustering Techniques, we gain a silhouette score of 0.525 at max covering 20.2% of total variance using `optimal_k (KMeans)=9` and `SVD_N=12`
2) Using UMAP and Guassian Mixture Models, we gain a silhouette score of ~80% with `num_clusters (GMM) = 9`, `n_components=2`, `min_dist=0.5`, `n_neighbors=40`, and `metric=cosine`.

Looking at the performance of both the pipelines, we could say that UMAP with GMM performs far efficiently than SVD + KMeans. But, UMAP and GMM are time consuming as compared to SVD and KMeans.
