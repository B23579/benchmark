library(tidyverse)
library(modelr)
library(glm2)
library(data.table)


my_data <-read.table("result_by_node_openmpi.csv")

open<-data_frame(bytes=my_data$V1,t_by_node = my_data$V3, Mbytes_by_node = my_data$V4)
my_data <-read.table("result_by_socket_openmpi.csv")

open <- mutate(open,t_by_socket=my_data$V3,Mbytes_by_socket=my_data$V4)

my_data <-read.table("result_same_socket_openmpi.csv")

open <- mutate(open,t_same_socket=my_data$V3,Mbytes_same_socket=my_data$V4)

my_data <-read.table("tcp.csv")

open <- mutate(open,t_tcp=my_data$V3,Mbytes_tcp=my_data$V4)

##########Fitting model for Lattency)

### 1 map by node)
model_by_node<-lm(t_by_node~bytes,open)

model_by_node
pred<-predict(model_by_node,data.frame(bytes=open$bytes))

open <- mutate(open,t_by_nodes_comp= pred)


model_by_socket<-lm(t_by_socket~bytes, open )
model_by_socket
pred<-predict(model_by_socket,data.frame(bytes=open$bytes))

model_same_socket<-lm(t_same_socket~bytes,open)
model_same_socket
pred<-predict(model_same_socket,data.frame(bytes=open$bytes))
pred

model_ob1<-lm(t_tcp~bytes,open)
model_same_socket
names(open)

fwrite(open, "Benchmark_openmpi.csv")
view(open)
###########plotting ##############

p<-ggplot(open)+
  geom_line(mapping = aes(x=log10(bytes),y=Mbytes_by_node, color="Map by node"))+
  geom_line(mapping = aes(x=log10(bytes),y=Mbytes_by_socket,color="Map by socket")) +
  geom_line(mapping = aes(x=log10(bytes),y=Mbytes_same_socket,color="Map same socket")) +
  geom_line(mapping = aes(x=log10(bytes),y=Mbytes_tcp,color="ob1")) +
  labs(x="Bytes in log10", y= "Bandwidth(Mbytes/sec)", title = "Openmpi Bandwith")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(name = "Topologies",
                     breaks = c( "Map by node","Map by socket","Map same socket","ob1"),
                     values = c( "Map by node"="Blue","Map by socket"="red","Map same socket"="black","ob1"="green"),drop=F )

ggsave(p, filename = "Bandwidth_openmpi.png")
p

p<-ggplot(open)+
  geom_line(mapping = aes(x=bytes,y=t_by_node, color="Map by node"))+
  geom_line(mapping = aes(x=bytes,y=t_by_socket,color="Map by socket")) +
  geom_line(mapping = aes(x=bytes,y=t_same_socket,color="Map same socket")) +
  geom_line(mapping = aes(x=bytes,y=t_tcp,color="ob1")) +
  geom_point(mapping = aes(x=bytes,y=t_by_nodes_comp,color="By node compute"))+
  labs(x="Bytes", y= "Latency(usec)", title = "Openmpi Latency")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(name = "Topologies",
                     breaks = c( "Map by node","Map by socket","Map same socket","ob1","By node compute"),
                     values = c( "Map by node"="Blue","Map by socket"="red","Map same socket"="black","ob1"="green","By node compute"="brown"),drop=F )

ggsave(p, filename = "Latency_openmpi.png")
p


################## Intel########################

my_data <-read.table("result_by_node_intel.csv")

open<-data_frame(bytes=my_data$V1,t_by_node = my_data$V3, Mbytes_by_node = my_data$V4)
my_data <-read.table("result_by_socket_intel.csv")

open <- mutate(open,t_by_socket=my_data$V3,Mbytes_by_socket=my_data$V4)

my_data <-read.table("result_by_core_intel.csv")

open <- mutate(open,t_by_core=my_data$V3,Mbytes_by_core=my_data$V4)

##########Fitting model for Lattency)

### 1 map by node)
model_by_node<-lm(t_by_node~bytes,open)
names(open)

model_by_node
pred<-predict(model_by_node,data.frame(bytes=open$bytes))

open <- mutate(open,t_by_nodes_comp= pred)


model_by_socket<-lm(t_by_socket~bytes, open )
model_by_socket
pred<-predict(model_by_socket,data.frame(bytes=open$bytes))
open <- mutate(open,t_by_socket_comp= pred)

model_by_core<-lm(t_by_core~bytes,open)
model_by_core

##########


#view(open)

fwrite(open, "Benchmark_intel.csv")

p<-ggplot(open)+
  geom_line(mapping = aes(x=log10(bytes),y=Mbytes_by_node, color="Map by node"))+
  geom_line(mapping = aes(x=log10(bytes),y=Mbytes_by_socket,color="Map by socket")) +
  geom_line(mapping = aes(x=log10(bytes),y=Mbytes_by_core,color="Map core")) +
  labs(x="Bytes in log10", y= "Bandwidth(Mbytes/sec)", title = " Intel Bandwith")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(name = "Topologies",
                     breaks = c( "Map by node","Map by socket","Map by core "),
                     values = c( "Map by node"="Blue","Map by socket"="red","Map by core "="black"),drop=F )

ggsave(p, filename = "Bandwidth_intel.png")
p

p<-ggplot(open)+
  geom_line(mapping = aes(x=bytes,y=t_by_node, color="Map by node"))+
  geom_line(mapping = aes(x=bytes,y=t_by_socket,color="Map by socket")) +
  geom_line(mapping = aes(x=bytes,y=t_by_core,color="Map by core")) +
  geom_point(mapping = aes(x=bytes,y=t_by_nodes_comp,color="By node compute"))+
  geom_point(mapping = aes(x=bytes,y=t_by_socket_comp,color="By socket compute"))+
  labs(x="Bytes", y= "Latency(usec)", title = "Intel Latency")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(name = "Topologies",
                     breaks = c( "Map by node","Map by socket","Map by core","By node compute","By socket compute"),
                     values = c( "Map by node"="Blue","Map by socket"="red","Map by core"="black","By node compute"="brown", "By socket compute"="darkviolet"),drop=F )

ggsave(p, filename = "Latency_intel.png")

p

########################dssc_gpu########################

my_data <-read.table("result_by_node_gpu.csv")

open<-data_frame(bytes=my_data$V1,t_by_node = my_data$V3, Mbytes_by_node = my_data$V4)

my_data <-read.table("result_same_socket_openmpi_gpu.csv")

open <- mutate(open,t_same_socket=my_data$V3,Mbytes_same_socket=my_data$V4)

my_data <-read.table("tcp_gpu.csv")

open <- mutate(open,t_tcp=my_data$V3,Mbytes_tcp=my_data$V4)

##########Fitting model for Lattency)

### 1 map by node)
model_by_node<-lm(t_by_node~bytes,open)

model_by_node
pred<-predict(model_by_node,data.frame(bytes=open$bytes))

open <- mutate(open,t_by_nodes_comp= pred)

model_same_socket<-lm(t_same_socket~bytes,open)
model_same_socket
pred<-predict(model_same_socket,data.frame(bytes=open$bytes))
pred

model_ob1<-lm(t_tcp~bytes,open)
model_same_socket
names(open)
###########

view(open)

fwrite(open, "Benchmark_openmpi_gpu.csv")

p<-ggplot(open)+
  geom_line(mapping = aes(x=log10(bytes),y=Mbytes_by_node, color="Map by node"))+
  geom_line(mapping = aes(x=log10(bytes),y=Mbytes_same_socket,color="Map same socket")) +
  geom_line(mapping = aes(x=log10(bytes),y=Mbytes_tcp,color="ob1")) +
  labs(x="Bytes in log10", y= "Bandwidth(Mbytes/sec)", title = "Openmpi Bandwith on dssc gpu")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(name = "Topologies",
                     breaks = c( "Map by node","Map same socket","ob1"),
                     values = c( "Map by node"="Blue","Map same socket"="black","ob1"="green"),drop=F )

ggsave(p, filename = "Bendwidth_openmpi_gpu.png")
p

p<-ggplot(open)+
  geom_line(mapping = aes(x=bytes,y=t_by_node, color="Map by node"))+
  geom_line(mapping = aes(x=bytes,y=t_same_socket,color="Map same socket")) +
  geom_line(mapping = aes(x=bytes,y=t_tcp,color="ob1")) +
  geom_point(mapping = aes(x=bytes,y=t_by_nodes_comp,color="By node compute"))+
  labs(x="Bytes", y= "Latency(usec)", title = "Openmpi Latency on dssc gpu")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(name = "Topologies",
                     breaks = c( "Map by node","Map same socket","ob1","By node compute"),
                     values = c( "Map by node"="Blue","Map same socket"="black","ob1"="green","By node compute"="darkviolet"),drop=F )

ggsave(p, filename = "Latency_openmpi_gpu.png")
p


################## Intel########################

my_data <-read.table("result_by_node_intel_gpu.csv")

open<-data_frame(bytes=my_data$V1,t_by_node = my_data$V3, Mbytes_by_node = my_data$V4)
my_data <-read.table("result_by_socket_intel_gpu.csv")

open <- mutate(open,t_by_socket=my_data$V3,Mbytes_by_socket=my_data$V4)

my_data <-read.table("result_by_core_intel_gpu.csv")

open <- mutate(open,t_by_core=my_data$V3,Mbytes_by_core=my_data$V4)

##########Fitting model for Lattency)

### 1 map by node)
model_by_node<-lm(t_by_node~bytes,open)
names(open)

model_by_node
pred<-predict(model_by_node,data.frame(bytes=open$bytes))

open <- mutate(open,t_by_nodes_comp= pred)


model_by_socket<-lm(t_by_socket~bytes, open )
model_by_socket
pred<-predict(model_by_socket,data.frame(bytes=open$bytes))
open <- mutate(open,t_by_socket_comp= pred)

model_by_core<-lm(t_by_core~bytes,open)
model_by_core

##########


view(open)
open<-data_frame(open)
fwrite(open, "Benchmark_intel_gpu.csv")

p<-ggplot(open)+
  geom_line(mapping = aes(x=log10(bytes),y=Mbytes_by_node, color="Map by node"))+
  geom_line(mapping = aes(x=log10(bytes),y=Mbytes_by_socket,color="Map by socket")) +
  geom_line(mapping = aes(x=log10(bytes),y=Mbytes_by_core,color="Map core")) +
  labs(x="Bytes in log10", y= "Bandwidth(Mbytes/sec)", title = " Intel Bandwith on dssc gpu")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(name = "Topologies",
                     breaks = c( "Map by node","Map by socket","Map by core "),
                     values = c( "Map by node"="Blue","Map by socket"="red","Map by core "="black"),drop=F )

ggsave(p, filename = "Bandwidth_intel_gpu.png")
p

p<-ggplot(open)+
  geom_line(mapping = aes(x=bytes,y=t_by_node, color="Map by node"))+
  geom_line(mapping = aes(x=bytes,y=t_by_socket,color="Map by socket")) +
  geom_line(mapping = aes(x=bytes,y=t_by_core,color="Map by core")) +
  geom_point(mapping = aes(x=bytes,y=t_by_nodes_comp,color="By node compute"))+
  geom_point(mapping = aes(x=bytes,y=t_by_socket_comp,color="By socket compute"))+
  labs(x="Bytes", y= "Latency(usec)", title = "Intel Latency on dssc gpu")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(name = "Topologies",
                     breaks = c( "Map by node","Map by socket","Map by core","By node compute","By socket compute"),
                     values = c( "Map by node"="Blue","Map by socket"="red","Map by core"="black","By node compute"="darkmagenta","By socket compute"="darkviolet"),drop=F )

ggsave(p, filename = "Latency_intel_gpu.png")
p

