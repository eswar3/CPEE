#!/usr/bin/python
from org.apache.pig.scripting import *

P = Pig.compile("""

previous_pagerank = 
    LOAD '$docs_in' 
    USING PigStorage('\t') 
    AS ( url: chararray, pagerank: float, links:{ link: ( url: chararray ) } );

outbound_pagerank = 
    FOREACH previous_pagerank 
    GENERATE 
        pagerank / COUNT ( links ) AS pagerank, 
        FLATTEN ( links ) AS to_url; 

new_pagerank = 
    FOREACH 
        ( COGROUP outbound_pagerank BY to_url, previous_pagerank BY url INNER )
    GENERATE 
        group AS url, 
        (( 1 - $d )/6) + $d * SUM ( outbound_pagerank.pagerank ) AS pagerank, 
        FLATTEN ( previous_pagerank.links ) AS links;
        
STORE new_pagerank 
    INTO '$docs_out' 
    USING PigStorage('\t');
""")

# d - damping factor
# docs_in - input file with data
params = { 'd': '0.5', 'docs_in': '/home/cloudera/Desktop/pig/Link' }

for i in range(10):
	out = "/home/cloudera/Desktop/pig/pagerank_data_" + str(i + 1)
	# Adding the parameter docs_out
	params["docs_out"] = out
	Pig.fs("rmr " + out)
	# Binding the parameters to PIG script
	stats = P.bind(params).runSingle()
	if not stats.isSuccessful():
		raise 'failed'
	# Changing the docs_in value to out value
	params["docs_in"] = out
