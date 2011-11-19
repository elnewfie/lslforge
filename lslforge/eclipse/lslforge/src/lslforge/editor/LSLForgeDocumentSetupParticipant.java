package lslforge.editor;

import lslforge.LSLForgePlugin;

import org.eclipse.core.filebuffers.IDocumentSetupParticipant;

import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IDocumentExtension3;
import org.eclipse.jface.text.IDocumentPartitioner;
import org.eclipse.jface.text.rules.FastPartitioner;

/**
 * 
 */
public class LSLForgeDocumentSetupParticipant implements IDocumentSetupParticipant {
	public void setup(IDocument document) {
		if (document instanceof IDocumentExtension3) {
			IDocumentExtension3 extension3= (IDocumentExtension3) document;
			IDocumentPartitioner partitioner= new FastPartitioner(LSLForgePlugin.getDefault().getLSLPartitionScanner(), LSLPartitionScanner.LSL_PARTITION_TYPES);
			extension3.setDocumentPartitioner(LSLForgePlugin.LSL_PARTITIONING, partitioner);
			partitioner.connect(document);
		}
	}
}
