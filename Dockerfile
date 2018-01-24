FROM swipl:7.5.12

# Run the image as a non-root user
RUN useradd -m -s /bin/sh myuser
USER myuser
WORKDIR /home/myuser

ADD reactome_utility_module.pl $HOME
ADD load.pl $HOME
ADD Homo_sapiens.owl $HOME
ADD logging.pl $HOME
ADD pre_links.pl $HOME
ADD probes.pl $HOME
ADD pathway_link.pl $HOME
ADD README.md $HOME

ENV PORT 4000
EXPOSE 4000


CMD swipl load.pl --no-fork --port=$PORT
