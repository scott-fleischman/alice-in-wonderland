FROM heroku/heroku:16

ENV LANG C.UTF-8

# Install required packages.
RUN apt-get update
RUN apt-get upgrade -y --assume-yes
# Install packages for stack and ghc.
RUN apt-get install -y --assume-yes xz-utils gcc libgmp-dev zlib1g-dev
# Install packages needed for libraries used by our app.
RUN apt-get install -y --assume-yes libpq-dev
# Install convenience utilities, like tree, ping, and vim.
RUN apt-get install -y --assume-yes tree iputils-ping vim-nox

# Remove apt caches to reduce the size of our container.
RUN rm -rf /var/lib/apt/lists/*

# Install stack to /opt/stack/bin.
RUN mkdir -p /opt/stack/bin
RUN curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C /opt/stack/bin '*/stack'

# Create /opt/alice/bin and /opt/alice/src.  Set
# /opt/alice/src as the working directory.
RUN mkdir -p /opt/alice/analysis
RUN mkdir -p /opt/alice/twitter
RUN mkdir -p /opt/alice/bin
WORKDIR /opt/alice/src

# Set the PATH for the root user so they can use stack.
ENV PATH "$PATH:/opt/stack/bin:/opt/alice/bin"

# Install GHC using stack, based on your app's stack.yaml file.
COPY ./stack.yaml /opt/alice/stack.yaml
RUN stack --no-terminal setup

# Install all dependencies in app's .cabal file.
COPY ./analysis /opt/alice/analysis
COPY ./twitter /opt/alice/twitter
RUN stack --no-terminal build alice-twitter --only-dependencies

# Build application.
RUN stack --no-terminal build alice-twitter

# Install application binaries to /opt/alice/bin.
RUN stack --no-terminal --local-bin-path /opt/alice/bin install alice-twitter

# Remove source code.
#RUN rm -rf /opt/alice/src

# Add the apiuser and setup their PATH.
RUN useradd -ms /bin/bash apiuser
RUN chown -R apiuser:apiuser /opt/alice
USER apiuser
ENV PATH "$PATH:/opt/stack/bin:/opt/alice/bin"

# Set the working directory as /opt/alice/.
WORKDIR /opt/alice

CMD /opt/alice/bin/alice-twitter
