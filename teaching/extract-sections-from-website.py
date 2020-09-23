# Making an edit.

# fname_prefix = 'ma-151-02-FA19'
fname_prefix = 'ma-237-01-FA20'


with open('{}_for_rescheduling.txt'.format(fname_prefix), 'w') as wfile:
	wfile.write('day\ttopics\tsections\n')
	with open('{}.html'.format(fname_prefix)) as ofile:
		line = ofile.readline()

		day_ind = 0

		while line != '':
			if ', Final Exam' in line:
				break

			line = ofile.readline()

			if ', Final Exam' in line:
				break

			while '<dd><b>Topics: </b>' not in line and line != '' and ', Final Exam' not in line:
				line = ofile.readline()

			if ', Final Exam' in line:
				break

			day_ind += 1

			topics = line.split('<dd><b>Topics: </b>')[1]

			topics_for_file = ' !'.join(topics.split('.')[:-1])

			print(topics_for_file)

			line = ofile.readline()

			if "Sections" in line:
				sections = line.split('<dd><b>Sections: </b>')[1]

				sections_for_file = ' ! '.join(sections.strip().split(', '))

				print()

				wfile.write('{}\t{}\t{}\n'.format(day_ind, topics_for_file, sections_for_file))
			else:
				print()

				wfile.write('{}\t{}\t\n'.format(day_ind, topics_for_file))