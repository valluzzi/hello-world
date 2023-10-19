FROM ubuntu:22.04

# Install Intel Fortran Compiler
COPY l_BaseKit_p_2023.2.0.49397_offline.sh .
RUN sh ./l_BaseKit_p_2023.2.0.49397_offline.sh