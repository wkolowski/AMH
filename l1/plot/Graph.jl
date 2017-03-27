using Gadfly
using DataFrames

function readData(filename::String)
	len = Array{Float64,1}([])
	push!(len, 0.0)
	name = ""
	open(filename) do file
		lines = readlines(file)
		name = strip(lines[1])
		for line in lines[2:end]
			push!(len, parse(Float64, strip(line)))
		end
	end


	df = DataFrame(time = 1:length(len), length = len)
	return df, name
end

function graph(df::DataFrame, name::String)
	draw(SVG("$name.svg", 6inch, 6inch), plot(df, x = "time", y = "length", Geom.line, Scale.color_discrete_manual("blue", "red")))
end




