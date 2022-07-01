# **Council Election Dash boards** 
The dashboards presented here are a sample of those that were used to help plan for, and run, the NSW Local Council 
Elections held on December 4th, 2021. Council Elections in NSW are a complex logistical exercise, involving the 
coordination of >25k staff across > 2.5k venues, and take a full year to plan and execute. The Data Analysis team
of the NSW Electoral Commission aids these processes by providing information summaries from different
sources (i.e. SQL databases and associated reports) using timely, efficient and automated processes (i.e. code
repositories to access databases routinely, process data into information and serve the summaries to business 
users at regular intervals). The Data team designed a series of dashboards in consultation with subject matter
experts, to show in real time if various electoral processes were on track. This site shows a sample of dashboards 
that were designed for these processes.



# **Candidate Nominations** 
The nominations process involves manual vetting of applications from major parties, minor parties and independents.
To summarise if the number of expected applications are being received and processed according to the tight schedule
required by legislation, several bespoke databases must be accessed, and the information synthesized across many
individual tables covering different topics. Approximately 3k nominations were received for LG21.



# **Declaration Voting** 
Under NSW legislation, several vote types are recognized as valid votes in either Council or general state elections.
Declaration votes are one such vote type, and are issued for various reasons (e.g. people who are voting outside their 
elecorate, are not on the roll, or may have already voted), and require extra manual processing. To summarise if the DecVotes 
processing is on track, several bespoke databases must again be accessed, and the information synthesized across many 
individual tables relating to vote type.


# **Ballot Tracking** 
Every ballot paper in a NSW election must be rigorously accounted for. Ballots papers are stored in cartons of various sizes 
(several hundred to several thousand - and every carton has a bar code, that is scanned through several logistical stages of 
the delivery process (from the printer to final vote counting). The dashboard interfaced with 3rd party software and EC databases 
to track if the number of expected cartons (which is know at the start of the election) was being scanned in and out at each stage.


# **Vote Counting** 
The Vote counting process has been rigorously developed over many years according to tight protocols to control for human error
and logistical bottlenecks. All cartons of ballot papers are delivered from regional offices (e.g. Parramatta) to central vote counting 
locations at Sydney and Newcastle. The vote counting is then split into two processes - batching and data entry.