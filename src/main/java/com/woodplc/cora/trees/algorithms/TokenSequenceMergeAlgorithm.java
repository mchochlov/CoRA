package com.woodplc.cora.trees.algorithms;

import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.TokenStreamRewriter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.github.gumtreediff.matchers.MappingStore;
import com.github.gumtreediff.matchers.Matcher;
import com.github.gumtreediff.matchers.Matchers;
import com.github.gumtreediff.tree.ITree;
import com.google.common.collect.MultimapBuilder.SetMultimapBuilder;
import com.google.common.collect.SetMultimap;
import com.woodplc.cora.data.OrderedLinkedMap;
import com.woodplc.cora.data.SubprogramMetadata;
import com.woodplc.cora.trees.FortranTreeGenerator;

final class TokenSequenceMergeAlgorithm implements CloneMergeAlgorithm {
	
	private enum UpdateType{
		INSERTED("INSERTED"), UPDATED("UPDATED");
		
		private final String label;
		private UpdateType(String label) {
			this.label = label;
		}

	};
	
	protected static final String NEW_LINE = "\r\n";
	private final OrderedLinkedMap<Path, Stream<String>> originalSubprograms;
	final Logger logger = LoggerFactory.getLogger(TokenSequenceMergeAlgorithm.class);
	private final boolean useSuperFunctionalityRefactoring;

	public TokenSequenceMergeAlgorithm(OrderedLinkedMap<Path, Stream<String>> originalSubprograms, boolean useSuperFunctionality) {
		this.originalSubprograms = originalSubprograms;
		this.useSuperFunctionalityRefactoring = useSuperFunctionality;
	}

	@Override
	public List<String> merge() throws IOException {
		if (originalSubprograms.size() == 0) {
			return new ArrayList<>();
		} else if (originalSubprograms.size() == 1) {
			return originalSubprograms.getValue(0).collect(Collectors.toList());
		} else {
			
			logger.info("Generating Fortran parse trees");
			CommonTokenStream cts = null;
			List<ITree> parseTrees = new ArrayList<>();
			for (int i = 0; i < originalSubprograms.size(); i++) {
				String original = originalSubprograms.getValue(i).map(String::toLowerCase).collect(Collectors.joining());
				if (i == 0) {
					FortranTreeGenerator ftg = new FortranTreeGenerator(originalSubprograms.getKey(i));
					parseTrees.add(ftg.generateFrom().string(original).getRoot());
					cts = ftg.getTokenStream();
				} else {
					parseTrees.add(new FortranTreeGenerator(originalSubprograms.getKey(i)).generateFrom().string(original).getRoot());
				}
			}
			
			List<SubprogramMetadata> smd = new ArrayList<>();
			Set<String> commonArguments = new TreeSet<>(String.CASE_INSENSITIVE_ORDER);
			if (useSuperFunctionalityRefactoring) {
				for (int i = 0; i < parseTrees.size(); i++) {
					smd.add(SubprogramMetadata.fromITree(parseTrees.get(i)));
				}
				commonArguments.addAll(smd.get(0).getArguments());
				for (int i = 1; i < parseTrees.size(); i++) {
					commonArguments.retainAll(smd.get(i).getArguments());
				}
			}
			
			logger.info("Matching ASTs");
			Matcher m = Matchers.getInstance().getMatcher();
			SetMultimap<ITree, MappingStore> treesToMapping = SetMultimapBuilder.hashKeys().hashSetValues().build();
			SetMultimap<ITree, ITree> combinedMappings = SetMultimapBuilder.hashKeys().hashSetValues().build();
			for (int i = 0; i < parseTrees.size() - 1; i++) {
				for (int j =  i + 1; j < parseTrees.size(); j++) {
					logger.info("Mapping tree {} to tree {}", i, j);
					MappingStore ms = m.match(parseTrees.get(i), parseTrees.get(j)); // return the mapping store	
					treesToMapping.put(parseTrees.get(i), ms);
					treesToMapping.put(parseTrees.get(j), ms);
					ms.forEach(x -> {
						combinedMappings.put(x.first, x.second);
						combinedMappings.put(x.second, x.first);
					});
				}
			}
			
			logger.info("Rewriting original subprogram");
			
			List<List<ITree>> leafSequences = new ArrayList<>();
			for (ITree root : parseTrees) {
				List<ITree> leaves = new ArrayList<>();
				for (ITree node : root.preOrder()) {
					if (node.isLeaf()) {
						leaves.add(node);
					}
				}
				leafSequences.add(leaves);
			}
			
			TokenStreamRewriter rewriter = new TokenStreamRewriter(cts);
			ITree rootA = parseTrees.get(0);
			List<ITree> leavesA = leafSequences.get(0);
			for (int i = 1; i < leafSequences.size(); i++) {
				ITree rootOther = parseTrees.get(i);
				List<ITree> leaves = leafSequences.get(i);
				Set<MappingStore> mappings = treesToMapping.get(rootOther);
				MappingStore ms = findMapping(rootA, rootOther, mappings);
				if (ms == null) {
					throw new IllegalStateException();
				}
				List<ITree> insertions = new ArrayList<>();
				Set<String> localArguments = new TreeSet<>(String.CASE_INSENSITIVE_ORDER);
				for (ITree leaf : leaves) {
					if (ms.isDstMapped(leaf)) {
						ITree srcTree = ms.getSrcForDst(leaf);
						if (!insertions.isEmpty()) {
							//rewrite
							if (useSuperFunctionalityRefactoring) {
								insert(rewriter, i, srcTree.getPos(), insertions, smd.get(i), commonArguments, localArguments);								
							} else {
								insert(rewriter, i, srcTree.getPos(), insertions);
							}

							//update mapped leaves
							updateMapped(mappings, insertions, UpdateType.INSERTED);
							insertions.clear();	
						}
						
						if (!srcTree.hasSameTypeAndLabel(leaf)) {
							update(rewriter, i, srcTree, leaf);
							updateMapped(mappings, List.of(leaf), UpdateType.UPDATED);
						}
					} else {
						UpdateType md = (UpdateType) leaf.getMetadata(UpdateType.INSERTED.label);
						if (md == null) {
							insertions.add(leaf);							
						}
					}
				}
				if (!insertions.isEmpty()) {
					//rewrite
					if (useSuperFunctionalityRefactoring) {
						insert(rewriter, i, leavesA.get(leavesA.size() - 1).getPos(), insertions, smd.get(i), commonArguments, localArguments);								
					} else {
						insert(rewriter, i, leavesA.get(leavesA.size() - 1).getPos(), insertions);
					}
					updateMapped(mappings, insertions, UpdateType.INSERTED);
				}
				
				commonArguments.addAll(localArguments);
				localArguments.clear();
			} 
			
			//System.out.println(commonArguments);
			// finally walk through the 0 tree
			List<ITree> insertions = new ArrayList<>();
			Set<String> localArguments = new TreeSet<>(String.CASE_INSENSITIVE_ORDER);
			for (ITree leaf : leafSequences.get(0)) {
				if (useSuperFunctionalityRefactoring) {
					if (combinedMappings.get(leaf).size() > 0 || smd.get(0).isSFRefactorable(leaf, commonArguments, localArguments)) {
						if (!insertions.isEmpty()) {
							String content = insertions.stream().map(ITree::getLabel).collect(Collectors.joining(" "));
							rewriter.replace(insertions.get(0).getPos(), insertions.get(insertions.size() - 1).getPos(), "<<INS from 0: " + content + ">>");
							insertions.clear();
						}
					} else {
						insertions.add(leaf);
					}

				} else {
					if (combinedMappings.get(leaf).size() > 0) {
						if (!insertions.isEmpty()) {
							String content = insertions.stream().map(ITree::getLabel).collect(Collectors.joining(" "));
							rewriter.replace(insertions.get(0).getPos(), insertions.get(insertions.size() - 1).getPos(), "<<INS from 0: " + content + ">>");
							insertions.clear();
						}
					} else {
						insertions.add(leaf);
					}

				}
			}
			if (!insertions.isEmpty()) {
				insert(rewriter, 0, leafSequences.get(0).get(leafSequences.get(0).size() - 1).getPos(), insertions);
			}
			
			return Arrays.asList(rewriter.getText().split(NEW_LINE));	
		}
	}

	private void update(TokenStreamRewriter rewriter, int tree, ITree srcTree, ITree leaf) {
		rewriter.replace(srcTree.getPos(), "<<UPD from " + tree + ": " + srcTree.getLabel() + " to " + leaf.getLabel() + ">>");
	}

	private void updateMapped(Set<MappingStore> mappings, List<ITree> insertions, UpdateType updateType) {
		for (MappingStore ms : mappings) {
			for (ITree leaf : insertions) {
				if (ms.isSrcMapped(leaf)) {
					ITree dst = ms.getDstForSrc(leaf);
					updateMetadata(dst, updateType);
				} else if (ms.isDstMapped(leaf)) {
					ITree dst = ms.getSrcForDst(leaf);
					updateMetadata(dst, updateType);
				}
			}
		}
		
	}

	private void updateMetadata(ITree dst, UpdateType updateType) {
		UpdateType type = (UpdateType) dst.getMetadata(updateType.label);
		if (type == null) {
			dst.setMetadata(updateType.label, updateType);
		}
	}
	
	private void insert(TokenStreamRewriter rewriter, int tree, int pos, List<ITree> insertions) {
		String content = insertions.stream().map(ITree::getLabel).collect(Collectors.joining(" "));
		rewriter.insertBefore(pos, "<<INS from " + tree + ": " + content + ">>");
	}
	
	private void insert(TokenStreamRewriter rewriter, int tree, int pos, List<ITree> insertions, SubprogramMetadata md,
			Set<String> commonArguments, Set<String> localArguments) {
		StringBuilder content = new StringBuilder();
		boolean arrows = false;
		for (ITree leaf : insertions) {
			if (md.isSFRefactorable(leaf, commonArguments, localArguments)) {
				if (arrows) {
					content.append(">>");
					arrows = !arrows;
				}
				content.append(leaf.getLabel()).append(" ");
			} else {
				if (!arrows) {
					content.append("<<INS from " + tree + ": ");
					arrows = !arrows;
				}
				content.append(leaf.getLabel()).append(" ");
			}
		}
		if (arrows) {
			content.append(">>");
		}
		rewriter.insertBefore(pos, content.toString());
	}

	private MappingStore findMapping(ITree rootA, ITree rootOther, Set<MappingStore> mappings) {
		for (MappingStore ms : mappings) {
			if (ms.src.equals(rootA) && ms.dst.equals(rootOther)) {
				return ms;
			}
		}
		return null;
	}

}
