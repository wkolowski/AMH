using Gadfly
using DataFrames

function readData(filename::String)
	len = Array{Float64,1}([])
	push!(len, 0.0)
	name = ""
	open(filename) do file
		lines = readlines(file)
		name = strip(lines[1])
		for line in lines[1:end]
			push!(len, parse(Float64, strip(line)))
		end
	end


	df = DataFrame(time = 0:length(len) - 1, length = len, label = filename)
	return df, name
end

function graph(df::DataFrame, name::String)
	draw(SVG("$name.svg", 6inch, 6inch), plot(df, x = "time", y = "length", color = "label", Geom.line, Scale.color_discrete_manual("blue", "red")))
end

function readAndPlot(filename::String)
	df, _ = readData(filename)
	graph(df, filename)
	run(`../conv.sh`)
end

function contraPlot(filename1::String, filename2::String)
	df1, _ = readData(filename1)
	df2, _ = readData(filename2)
	df = vcat(df1, df2)
	graph(df, filename1 * "_" * filename2)
	run(`../conv.sh`)
end

function plotAll(filenames)
	df1, _ = readData(filenames[1])
	df2, _ = readData(filenames[2])
	df = vcat(df1, df2)
	for filename in filenames[3:end]
		df_new, _ = readData(filename)
		df = vcat(df, df_new)
	end
	graph(df, "allPlot")
	run(`../conv.sh`)
end


