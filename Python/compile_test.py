import torch_compile.utils.checkpointing as checkpointing
import torch_compile.utils.src_builder   as src_builder
import torch_models.vision_models as vision_models
import torch


def main():
    model = vision_models.DenseNet()
    inp = torch.randn([64, 3, 224, 224])
    output = checkpointing.auto_checkpoint(model, inp, 16384000000, verbose=True)
    with open('densenet.py', 'w') as fp:
        fp.write(src_builder.to_python_src('DenseNet', output.params, output.start,\
                                           output.graph, output.checkpoints))

main()